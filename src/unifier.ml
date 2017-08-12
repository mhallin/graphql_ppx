open Ast
open Source_pos
open Schema

open Ast_404
open Parsetree
open Asttypes

exception Unimplemented of string

type native_type_ref = 
  | Ntr_named of string
  | Ntr_nullable of native_type_ref
  | Ntr_list of native_type_ref

let rec to_native_type_ref tr = match tr with
  | NonNull (Named n) -> Ntr_named n
  | NonNull (List l) -> Ntr_list (to_native_type_ref l)
  | NonNull i -> to_native_type_ref i
  | List l -> Ntr_nullable (Ntr_list (to_native_type_ref l))
  | Named n -> Ntr_nullable (Ntr_named n)

let rec to_schema_type_ref tr = match tr with
  | Tr_list l -> List (to_schema_type_ref l.item)
  | Tr_named n -> Named n.item
  | Tr_non_null_list l -> NonNull (List (to_schema_type_ref l.item))
  | Tr_non_null_named n -> NonNull (Named n.item)

let raise_error map_loc span message =
  raise (Location.Error (
      Location.error ~loc:(map_loc span) message
    ))

let some_or o d = match o with
  | Some v -> v
  | None -> d

let make_simple_type name loc =
  { ptyp_desc = 
      Ptyp_constr ({ txt = Longident.parse name; loc = loc}, [])
  ; ptyp_loc = loc; ptyp_attributes = [] }

let make_error_raiser loc = 
  Pexp_apply ({pexp_desc = Pexp_ident {txt = Longident.Lident "raise"; loc = loc};
               pexp_loc = loc; pexp_attributes = []},
              [(Nolabel, 
                {pexp_desc = Pexp_construct ({txt = Longident.Lident "Graphql_error"; loc = loc},
                                             None);
                 pexp_loc = loc; pexp_attributes = []})])

let make_match_fun loc decoder none_arm some_arm = 
  Pexp_match (
    {pexp_desc = Pexp_apply ({pexp_desc = Pexp_ident{txt = Longident.parse decoder; loc = loc};
                              pexp_loc = loc; pexp_attributes = []},
                             [(Nolabel, {pexp_desc = Pexp_ident {txt=Longident.Lident "value"; loc = loc};
                                         pexp_loc = loc; pexp_attributes = []})]);
     pexp_loc = loc; pexp_attributes = []},
    [
      {pc_lhs = {ppat_desc = Ppat_construct ({txt = Longident.Lident "None"; loc = loc}, None);
                 ppat_loc = loc; ppat_attributes = []};
       pc_guard = None;
       pc_rhs = {pexp_desc = none_arm;
                 pexp_loc = loc; pexp_attributes = []}
      };
      {
        pc_lhs = {ppat_desc = Ppat_construct ({txt = Longident.Lident "Some"; loc = loc},
                                              Some {ppat_desc = Ppat_var { txt = "value"; loc = loc};
                                                    ppat_loc = loc; ppat_attributes = []});
                  ppat_loc = loc; ppat_attributes = [];};
        pc_guard = None;
        pc_rhs = {
          pexp_desc = some_arm;
          pexp_loc = loc;
          pexp_attributes = [];
        };
      }
    ]
  )

let mangle_enum_name = String.uncapitalize

let rec unify_type map_loc span ty schema (selection_set: selection list spanning option) =
  let loc = map_loc span in
  match ty with
  | Ntr_nullable t ->
    make_match_fun loc "Js.Json.decodeNull" 
      (Pexp_construct ({ txt = Longident.Lident "Some"; loc = loc }, Some {
           pexp_desc = (unify_type map_loc span t schema selection_set);
           pexp_loc = loc;
           pexp_attributes = [];
         }))
      (Pexp_construct ({ txt = Longident.Lident "None"; loc = loc }, None))
  | Ntr_list t ->
    make_match_fun loc "Js.Json.decodeArray" (make_error_raiser loc)
      (Pexp_apply (
          {pexp_desc = Pexp_ident { txt = Longident.parse "Array.map"; loc = loc };
           pexp_loc = loc; pexp_attributes = []},
          [(Nolabel, {
               pexp_desc = Pexp_fun (
                   Nolabel,
                   None,
                   {ppat_desc = Ppat_var { txt = "value"; loc = loc }; ppat_loc = loc; ppat_attributes = []},
                   {pexp_desc = (unify_type map_loc span t schema selection_set);
                    pexp_loc = loc; pexp_attributes = []}
                 );
               pexp_loc = loc; pexp_attributes = []});
           (Nolabel, {
               pexp_desc = Pexp_ident { txt = Longident.Lident "value"; loc = loc };
               pexp_loc = loc; pexp_attributes = [];
             })]
        ))
  | Ntr_named n -> match lookup_type schema n with
    | None -> raise_error map_loc span ("Could not find type " ^ n)
    | Some Scalar { sm_name = "String" } ->
      make_match_fun loc "Js.Json.decodeString" (make_error_raiser loc)
        (Pexp_constraint ({
             pexp_desc = Pexp_ident { txt = Longident.Lident "value"; loc = loc};
             pexp_loc = loc;
             pexp_attributes = []
           }, {
               ptyp_desc = Ptyp_constr ({ txt = Longident.Lident "string"; loc = loc }, []);
               ptyp_loc = loc;
               ptyp_attributes = [];
             }))
    | Some Scalar { sm_name = "Int" } ->
      make_match_fun loc "Js.Json.decodeNumber" (make_error_raiser loc)
        (Pexp_apply ({pexp_desc = Pexp_ident{txt = Longident.Lident "int_of_float"; loc = loc};
                      pexp_loc = loc; pexp_attributes = []},
                     [(Nolabel, {pexp_desc = Pexp_ident {txt=Longident.Lident "value"; loc = loc};
                                 pexp_loc = loc; pexp_attributes = []})]))
    | Some Scalar { sm_name = "Float" } ->
      make_match_fun loc "Js.Json.decodeNumber" (make_error_raiser loc)
        (Pexp_ident {txt=Longident.Lident "value"; loc = loc})
    | Some Scalar _ -> raise_error map_loc span ("Unknown scalar type " ^ n)
    | Some ((Object o) as ty) ->
      unify_selection_set map_loc span schema ty selection_set
    | Some Enum { em_name; em_values } ->
      let enum_ty = {
        ptyp_desc = Ptyp_variant (
            List.map (fun { evm_name } -> Rtag (evm_name, [], true, [])) em_values,
            Closed, None
          );
        ptyp_loc = loc;
        ptyp_attributes = [];
      }
      in
      let enum_vals =
        Pexp_match (
          {pexp_desc = Pexp_ident { txt = Longident.Lident "value"; loc = loc};
           pexp_loc = loc; pexp_attributes = []},
          List.concat [
            List.map (fun { evm_name } -> {
                  pc_lhs = {ppat_desc = Ppat_constant (Pconst_string (evm_name, None)); ppat_loc = loc; ppat_attributes = []};
                  pc_guard = None;
                  pc_rhs = {
                    pexp_desc = Pexp_variant (evm_name, None);
                    pexp_loc = loc;
                    pexp_attributes = [];
                  }
                }) em_values;
            [{
              pc_lhs = {ppat_desc = Ppat_any; ppat_loc = loc; ppat_attributes = []};
              pc_guard = None;
              pc_rhs = {
                pexp_desc = make_error_raiser loc;
                pexp_loc = loc;
                pexp_attributes = [];
              }
            }]
          ]
        )
      in
      make_match_fun loc "Js.Json.decodeString" (make_error_raiser loc)
        (Pexp_constraint ({pexp_desc = enum_vals; pexp_loc = loc; pexp_attributes = []},
                          enum_ty))
    | Some ((Interface o) as ty) ->
      unify_selection_set map_loc span schema ty selection_set
    | Some InputObject _ -> raise_error map_loc span "Can't have fields on input objects"
    | Some Union _ -> raise_error map_loc span "Unions are not supported yet"

and unify_variant map_loc span ty schema selection_set =
  let loc = map_loc span in
  let rec match_loop ty selection_set = match selection_set with
    | [] -> make_error_raiser loc
    | Field { item; span } :: tl -> begin
        match lookup_field ty item.fd_name.item with
        | None -> raise_error map_loc span ("Unknown field on type " ^ type_name ty)
        | Some field_meta ->
          let key = (some_or item.fd_alias item.fd_name).item in
          let inner_type = match (to_native_type_ref field_meta.fm_field_type) with
            | Ntr_list _ | Ntr_named _ -> raise_error map_loc span "Variant field must only contain nullable fields"
            | Ntr_nullable i -> i in
          Pexp_let (
            Nonrecursive,
            [
              {
                pvb_pat = {ppat_desc = Ppat_var { txt = "temp"; loc = loc };
                           ppat_loc = loc; ppat_attributes = []};
                pvb_expr = {
                  pexp_desc = Pexp_apply ({
                      pexp_desc = Pexp_ident {txt = Longident.parse "Js.Dict.unsafeGet"; loc = loc};
                      pexp_loc = loc;
                      pexp_attributes = [];
                    }, [
                        (Nolabel, { pexp_desc = Pexp_ident { txt = Longident.Lident "value"; loc = loc};
                                    pexp_loc = loc; pexp_attributes = []});
                        (Nolabel, { pexp_desc = Pexp_constant (Pconst_string (key, None));
                                    pexp_loc = loc; pexp_attributes = []});
                      ]);
                  pexp_loc = loc;
                  pexp_attributes = [];
                };
                pvb_loc = loc;
                pvb_attributes = [];
              }
            ],
            {
              pexp_desc = Pexp_match (
                  {
                    pexp_desc = Pexp_apply ({
                        pexp_desc = Pexp_ident {txt = Longident.parse "Js.Json.decodeNull"; loc = loc};
                        pexp_loc = loc;
                        pexp_attributes = [];
                      },
                        [
                          (Nolabel, { pexp_desc = Pexp_ident { txt = Longident.Lident "temp"; loc = loc};
                                      pexp_loc = loc; pexp_attributes = []});
                        ]);
                    pexp_loc = loc;
                    pexp_attributes = [];
                  },
                  [
                    {
                      pc_lhs = {
                        ppat_desc = Ppat_construct ({txt = Longident.parse "Some"; loc = loc}, 
                                                    Some {ppat_desc = Ppat_any;
                                                          ppat_loc = loc;
                                                          ppat_attributes = []});
                        ppat_loc = loc; ppat_attributes = []};
                      pc_guard = None;
                      pc_rhs = {
                        pexp_desc = match_loop ty tl;
                        pexp_loc = loc;
                        pexp_attributes = [];
                      };
                    };
                    {
                      pc_lhs = {
                        ppat_desc = Ppat_construct ({ txt = Longident.parse "None"; loc = loc }, None);
                        ppat_loc = loc; ppat_attributes = [];};
                      pc_guard = None;
                      pc_rhs = {
                        pexp_desc = Pexp_let (
                            Nonrecursive,
                            [
                              {
                                pvb_pat = {ppat_desc = Ppat_var { txt = "value"; loc = loc };
                                           ppat_loc = loc; ppat_attributes = []};
                                pvb_expr = {
                                  pexp_desc = Pexp_ident { txt = Longident.Lident "temp"; loc = loc};
                                  pexp_loc = loc;
                                  pexp_attributes = [];
                                };
                                pvb_loc = loc;
                                pvb_attributes = [];
                              }
                            ],
                            {pexp_desc = 
                               Pexp_variant (
                                 String.capitalize key,
                                 Some {
                                   pexp_desc = unify_type map_loc span inner_type schema item.fd_selection_set;
                                   pexp_loc = loc;
                                   pexp_attributes = [];
                                 }
                               );
                             pexp_loc = loc;
                             pexp_attributes = []
                            });
                        pexp_loc = loc;
                        pexp_attributes = [];
                      };
                    };
                  ]
                );
              pexp_loc = loc;
              pexp_attributes = [];
            })
      end
    | FragmentSpread { span } :: _ -> raise_error map_loc span "Variant selections can only contain fields"
    | InlineFragment { span } :: _ -> raise_error map_loc span "Variant selections can only contain fields"
  in
  match ty with
  | Ntr_nullable t -> 
    make_match_fun loc "Js.Json.decodeNull" 
      (Pexp_construct ({ txt = Longident.Lident "Some"; loc = loc }, Some {
           pexp_desc = (unify_variant map_loc span t schema selection_set);
           pexp_loc = loc;
           pexp_attributes = [];
         }))
      (Pexp_construct ({ txt = Longident.Lident "None"; loc = loc }, None))
  | Ntr_list t ->
    make_match_fun loc "Js.Json.decodeArray" (make_error_raiser loc)
      (Pexp_apply (
          {pexp_desc = Pexp_ident { txt = Longident.parse "Array.map"; loc = loc };
           pexp_loc = loc; pexp_attributes = []},
          [(Nolabel, {
               pexp_desc = Pexp_fun (
                   Nolabel,
                   None,
                   {ppat_desc = Ppat_var { txt = "value"; loc = loc }; ppat_loc = loc; ppat_attributes = []},
                   {pexp_desc = (unify_variant map_loc span t schema selection_set);
                    pexp_loc = loc; pexp_attributes = []}
                 );
               pexp_loc = loc; pexp_attributes = []});
           (Nolabel, {
               pexp_desc = Pexp_ident { txt = Longident.Lident "value"; loc = loc };
               pexp_loc = loc; pexp_attributes = [];
             })]
        ))
  | Ntr_named n -> match lookup_type schema n with
    | None -> raise_error map_loc span ("Could not find type " ^ n)
    | Some Scalar _ -> raise_error map_loc span "Variant fields can only be applied to object types"
    | Some Enum _ -> raise_error map_loc span "Variant fields can only be applied to object types"
    | Some Interface _ -> raise_error map_loc span "Variant fields can only be applied to object types"
    | Some Union _ -> raise_error map_loc span "Variant fields can only be applied to object types"
    | Some InputObject _ -> raise_error map_loc span "Variant fields can only be applied to object types"
    | Some ((Object _) as ty) ->
      match selection_set with
      | Some { item } -> 
        make_match_fun loc "Js.Json.decodeObject" (make_error_raiser loc)
          (match_loop ty item)
      | None -> raise_error map_loc span "Variant fields need a selection set"

and unify_field map_loc field_span ty schema =
  let ast_field = field_span.item in
  let field_meta = lookup_field ty ast_field.fd_name.item in
  let key = (some_or ast_field.fd_alias ast_field.fd_name).item in
  let loc = map_loc field_span.span in
  let is_variant = List.exists (fun { item = { d_name = { item } } } -> item = "bsVariant") ast_field.fd_directives in
  let sub_unifier = if is_variant then unify_variant else unify_type in
  match field_meta with
  | None -> raise_error map_loc field_span.span ("Unknown field on type " ^ type_name ty)
  | Some field_meta ->
    (
      { txt = Longident.Lident key; loc = loc },
      {
        pexp_desc = Pexp_let (
            Nonrecursive, 
            [{pvb_pat = {ppat_desc = Ppat_var {txt="value"; loc = loc}; ppat_loc = loc; ppat_attributes = []};
              pvb_loc = loc;
              pvb_attributes = [];
              pvb_expr = {
                pexp_desc = Pexp_apply ({
                    pexp_desc = Pexp_ident {txt = Longident.parse "Js.Dict.unsafeGet"; loc = loc};
                    pexp_loc = loc;
                    pexp_attributes = [];
                  },
                    [
                      (Nolabel, { pexp_desc = Pexp_ident { txt = Longident.Lident "value"; loc = loc};
                                  pexp_loc = loc; pexp_attributes = []});
                      (Nolabel, { pexp_desc = Pexp_constant (Pconst_string (key, None));
                                  pexp_loc = loc; pexp_attributes = []});
                    ]);
                pexp_loc = loc;
                pexp_attributes = [];
              }}],
            {
              pexp_desc = sub_unifier map_loc field_span.span (to_native_type_ref field_meta.fm_field_type) schema ast_field.fd_selection_set;
              pexp_loc = loc;
              pexp_attributes = [];
            });
        pexp_loc = loc;
        pexp_attributes = [];
      }
    )

and unify_selection map_loc schema ty selection = match selection with
  | Field field_span -> unify_field map_loc field_span ty schema
  | FragmentSpread _ -> raise @@ Unimplemented "fragment spreads"
  | InlineFragment _ -> raise @@ Unimplemented "inline fragments"

and unify_selection_set map_loc span schema ty selection_set = match selection_set with
  | None -> raise_error map_loc span "Must select subfields on objects"
  | Some { item } -> let loc = map_loc span in
    make_match_fun loc "Js.Json.decodeObject" (make_error_raiser loc)
      (Pexp_extension (
          {txt = "bs.obj"; loc = loc},
          PStr [{
              pstr_desc = Pstr_eval (
                  {pexp_desc = Pexp_record (
                       (List.map (unify_selection map_loc schema ty) item),
                       None);
                   pexp_loc = loc;
                   pexp_attributes = []},
                  []);
              pstr_loc = loc;
            }]
        ))

let unify_document_schema map_loc schema document =
  match document with
  | [Operation { item = { o_type = Query; o_selection_set }; span } ] ->
    unify_selection_set map_loc span schema (query_type schema) (Some o_selection_set)
  | [Operation { item = { o_type = Mutation; o_selection_set }; span } ] -> begin match mutation_type schema with
      | Some mutation_type -> 
        unify_selection_set map_loc span schema mutation_type (Some o_selection_set)
      | None ->
        raise_error map_loc span "This schema does not contain any mutations"
    end
  | _ -> raise @@ Unimplemented "unification with other than singular queries"

let rec convert_arg_to_json map_loc name var_type =
  let name_loc = map_loc name.span in
  match var_type with
  | Ntr_nullable inner ->
    Pexp_match (
      {pexp_desc = Pexp_ident { txt = Longident.Lident name.item; loc = name_loc };
       pexp_loc = name_loc; pexp_attributes = []},
      [
        {pc_lhs = {ppat_desc = Ppat_construct ({txt = Longident.Lident "None"; loc = name_loc}, None);
                   ppat_loc = name_loc; ppat_attributes = []};
         pc_guard = None;
         pc_rhs = {pexp_desc = Pexp_ident { txt = Longident.parse "Js.Json.null"; loc = name_loc };
                   pexp_loc = name_loc; pexp_attributes = []}
        };
        {
          pc_lhs = {ppat_desc = Ppat_construct ({txt = Longident.Lident "Some"; loc = name_loc},
                                                Some {ppat_desc = Ppat_var { txt = name.item; loc = name_loc};
                                                      ppat_loc = name_loc; ppat_attributes = []});
                    ppat_loc = name_loc; ppat_attributes = [];};
          pc_guard = None;
          pc_rhs = {
            pexp_desc = convert_arg_to_json map_loc name inner;
            pexp_loc = name_loc;
            pexp_attributes = [];
          };
        };
      ]
    )
  | Ntr_named "String" ->
    Pexp_apply (
      {pexp_desc = Pexp_ident { txt = Longident.parse "Js.Json.string"; loc = name_loc};
       pexp_loc = name_loc; pexp_attributes = []},
      [
        (Nolabel, {
            pexp_desc = Pexp_ident { txt = Longident.Lident name.item; loc = name_loc};
            pexp_loc = name_loc; pexp_attributes = [];
          });
      ]
    )
  | Ntr_named "Float" ->
    Pexp_apply (
      {pexp_desc = Pexp_ident { txt = Longident.parse "Js.Json.float"; loc = name_loc};
       pexp_loc = name_loc; pexp_attributes = []},
      [
        (Nolabel, {
            pexp_desc = Pexp_ident { txt = Longident.Lident name.item; loc = name_loc};
            pexp_loc = name_loc; pexp_attributes = [];
          });
      ]
    )
  | Ntr_named "Int" ->
    Pexp_apply (
      {pexp_desc = Pexp_ident { txt = Longident.parse "Js.Json.float"; loc = name_loc};
       pexp_loc = name_loc; pexp_attributes = []},
      [
        (Nolabel, {
            pexp_desc = Pexp_apply (
                {pexp_desc = Pexp_ident { txt = Longident.Lident "int_of_float"; loc = name_loc};
                 pexp_loc = name_loc; pexp_attributes = []},
                [
                  (Nolabel, {
                      pexp_desc = Pexp_ident { txt = Longident.Lident name.item; loc = name_loc};
                      pexp_loc = name_loc; pexp_attributes = [];
                    });
                ]
              );
            pexp_loc = name_loc; pexp_attributes = [];
          })
      ]
    )
  | Ntr_named n -> raise_error map_loc name.span "Unsupported input type"
  | Ntr_list l -> raise_error map_loc name.span "Unsupported input type"

let rec make_make_fun map_loc schema document =
  let make_make_triple loc variables =
    (Pexp_extension (
        {txt = "bs.obj"; loc = loc},
        PStr [
          {
            pstr_desc = Pstr_eval (
                {pexp_desc = Pexp_record ([(
                     { txt = Longident.Lident "query"; loc = loc },
                     {
                       pexp_desc = Pexp_ident { txt = Longident.Lident "query"; loc = loc};
                       pexp_loc = loc;
                       pexp_attributes = [];
                     }
                   );
                    (
                      { txt = Longident.Lident "variables"; loc = loc },
                      {
                        pexp_desc = variables;
                        pexp_loc = loc;
                        pexp_attributes = [];
                      }
                    );
                    (
                      { txt = Longident.Lident "parse"; loc = loc },
                      {
                        pexp_desc = Pexp_ident { txt = Longident.Lident "parse"; loc = loc};
                        pexp_loc = loc;
                        pexp_attributes = [];
                      }
                    )
                   ], None);
                 pexp_loc = loc;
                 pexp_attributes = [];
                }, []);
            pstr_loc =loc;
          }]
      )) in
  match document with
  | [Operation { item = { o_variable_definitions = Some { item; span } }}] -> begin
      let rec make_labelled_function defs body = match defs with
        | [] -> Pexp_fun (
            Nolabel,
            None,
            {ppat_desc = Ppat_construct ({ txt = Longident.Lident "()"; loc = map_loc span }, None );
             ppat_loc = map_loc span; ppat_attributes = []},
            { pexp_desc = body; pexp_loc = map_loc span; pexp_attributes = []; })
        | (name, def) :: tl -> let name_loc = map_loc name.span in 
          Pexp_fun (
            (match def.vd_type.item with
             | Tr_non_null_list _ | Tr_non_null_named _ -> Labelled name.item
             | Tr_list _ | Tr_named _ -> Optional name.item),
            None,
            {ppat_desc = Ppat_var { txt = name.item; loc = name_loc}; ppat_loc = name_loc; ppat_attributes = []},
            {
              pexp_desc = make_labelled_function tl body;
              pexp_loc = name_loc;
              pexp_attributes = [];
            }
          ) in
      let rec make_body defs = match defs with
        | (name, def) :: tl -> Pexp_construct (
            { txt = Longident.Lident "::"; loc = map_loc name.span },
            Some {
              pexp_desc = Pexp_tuple [
                  {
                    pexp_desc = Pexp_tuple [
                        {
                          pexp_desc = Pexp_constant (Pconst_string (name.item, None));
                          pexp_loc = map_loc name.span;
                          pexp_attributes = [];
                        };
                        {
                          pexp_desc = convert_arg_to_json map_loc name (to_native_type_ref (to_schema_type_ref def.vd_type.item));
                          pexp_loc = map_loc name.span;
                          pexp_attributes = [];
                        };
                      ];
                    pexp_loc = map_loc name.span;
                    pexp_attributes = [];
                  };
                  {
                    pexp_desc = make_body tl;
                    pexp_loc = map_loc name.span;
                    pexp_attributes = [];
                  };
                ];
              pexp_loc = map_loc name.span;
              pexp_attributes = [];
            }
          )
        | [] -> Pexp_construct ({ txt = Longident.Lident "[]"; loc = map_loc span}, None)
      in
      let loc = map_loc span in
      let variable_ctor_body = 
        Pexp_apply (
          {pexp_desc = Pexp_ident { txt = Longident.parse "Js.Json.object_"; loc = loc };
           pexp_loc = loc; pexp_attributes = []},
          [(Nolabel, {pexp_desc = Pexp_apply ({
               pexp_desc = Pexp_ident { txt = Longident.parse "Js.Dict.fromList"; loc = loc };
               pexp_loc = loc; pexp_attributes = []}
               ,
               [(Nolabel, {pexp_desc = (make_body item); pexp_loc = loc; pexp_attributes = []})]);
              pexp_loc = loc; pexp_attributes = []})]) in
      make_labelled_function item (make_make_triple loc variable_ctor_body )
    end
  | [Operation { item = { o_variable_definitions = None }; span }] -> begin
      let loc = map_loc span in
      Pexp_fun (
        Nolabel,
        None,
        {ppat_desc = Ppat_construct ({ txt = Longident.Lident "()"; loc = map_loc span }, None );
         ppat_loc = map_loc span; ppat_attributes = []},
        { pexp_desc = make_make_triple loc (Pexp_ident { txt = Longident.parse "Js.Json.null"; loc = loc });
          pexp_loc = map_loc span; pexp_attributes = []; })
    end
  | _ -> raise @@ Unimplemented "variables on other than singular queries/mutations"
