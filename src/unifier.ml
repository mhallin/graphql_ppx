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

let rec type_name_of_native_type_ref = 
  function
  | Ntr_named s -> s
  | Ntr_nullable x -> type_name_of_native_type_ref x
  | Ntr_list x -> type_name_of_native_type_ref x

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

let make_expression loc pexp_desc  = {
  pexp_desc; 
  pexp_loc = loc;
  pexp_attributes = [];
} 
let make_pattern loc ppat_desc = {
  ppat_desc;
  ppat_loc = loc;
  ppat_attributes = [];
} 
let make_type_from_string loc t = {
  ptyp_desc = Ptyp_constr ({ txt = Longident.Lident t; loc = loc }, []);
  ptyp_loc = loc;
  ptyp_attributes = [];
} 

let make_type loc t = {
  ptyp_desc = t;
  ptyp_loc = loc;
  ptyp_attributes = [];
} 

let make_value_expression loc identifier = 
  make_expression loc (Pexp_ident {txt=Longident.Lident identifier; loc = loc})
let make_expression_from_string loc func_name =
  (Pexp_ident { txt = Longident.parse func_name; loc })

let rec unify_type map_loc span ty schema (selection_set: selection list spanning option) =
  let loc = map_loc span in
  let make_match_fun = make_match_fun loc in
  let make_expression = make_expression loc in
  let make_type = make_type_from_string loc in
  let make_value_expression = make_value_expression loc in
  let make_expression_from_string = make_expression_from_string loc in
  let make_pattern = make_pattern loc in
  match ty with
  | Ntr_nullable t ->
    make_match_fun "Js.Json.decodeNull" 
      (Pexp_construct ({ txt = Longident.Lident "Some"; loc = loc }, 
                       Some (make_expression (unify_type map_loc span t schema selection_set))))
      (Pexp_construct ({ txt = Longident.Lident "None"; loc = loc }, None))
  | Ntr_list t -> 
    make_match_fun "Js.Json.decodeArray" (make_error_raiser loc)
      (Pexp_apply (
          make_expression (make_expression_from_string "Array.map"),
          [(Nolabel, make_expression (
               Pexp_fun (
                 Nolabel,
                 None,
                 make_pattern (Ppat_var { txt = "value"; loc = loc }),
                 make_expression (unify_type map_loc span t schema selection_set)
               );
             ));
           (Nolabel, make_value_expression "value")]
        ))
  | Ntr_named n -> match lookup_type schema n with
    | None -> raise_error map_loc span ("Could not find type " ^ n)
    | Some Scalar { sm_name = "ID" } 
    | Some Scalar { sm_name = "String" } ->
      make_match_fun "Js.Json.decodeString" (make_error_raiser loc)
        (Pexp_constraint (make_value_expression "value", make_type "string"))
    | Some Scalar { sm_name = "Int" } ->
      make_match_fun "Js.Json.decodeNumber" (make_error_raiser loc)
        (Pexp_apply (
            make_value_expression "int_of_float",
            [(Nolabel, make_value_expression "value")]))
    | Some Scalar { sm_name = "Float" } ->
      make_match_fun "Js.Json.decodeNumber" (make_error_raiser loc)
        (Pexp_ident {txt=Longident.Lident "value"; loc = loc})
    | Some Scalar { sm_name = "Boolean" } ->
      make_match_fun "Js.Json.decodeBoolean" (make_error_raiser loc)
        (Pexp_ident {txt=Longident.Lident "value"; loc = loc})
    | Some Scalar _ -> 
      (Pexp_ident {txt=Longident.Lident "value"; loc = loc})
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
          make_value_expression "value",
          List.concat [
            List.map (fun { evm_name } -> {
                  pc_lhs = {ppat_desc = Ppat_constant (Pconst_string (evm_name, None)); ppat_loc = loc; ppat_attributes = []};
                  pc_guard = None;
                  pc_rhs = make_expression (Pexp_variant (evm_name, None)) 
                }) em_values;
            [{
              pc_lhs = {ppat_desc = Ppat_any; ppat_loc = loc; ppat_attributes = []};
              pc_guard = None;
              pc_rhs = make_expression (make_error_raiser loc)
            }]
          ]
        )
      in
      make_match_fun "Js.Json.decodeString" (make_error_raiser loc)
        (Pexp_constraint (make_expression enum_vals, enum_ty))
    | Some ((Interface o) as ty) ->
      unify_selection_set map_loc span schema ty selection_set
    | Some InputObject obj -> raise_error map_loc span "Can't have fields on input objects"
    | Some Union _ -> raise_error map_loc span "Unions are not supported yet"

and unify_variant map_loc span ty schema selection_set =
  let loc = map_loc span in
  let make_expression_from_string = make_expression_from_string loc in
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
                      pexp_desc = make_expression_from_string "Js.Dict.unsafeGet";
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
                        pexp_desc = make_expression_from_string "Js.Json.decodeNull";
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
          {pexp_desc = make_expression_from_string "Array.map";
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
  let make_expression_from_string = make_expression_from_string loc in
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
                    pexp_desc = make_expression_from_string "Js.Dict.unsafeGet";
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

let apply loc func_name expression =
  Pexp_apply (
    make_expression loc (make_expression_from_string loc func_name),
    [(Nolabel, expression)]
  )

let apply_multi loc func_name expressions =
  Pexp_apply (
    make_expression loc (Pexp_ident { txt = Longident.parse func_name; loc}),
    List.map (fun expression -> (Nolabel, expression)) expressions
  )

let apply_js loc x y =
  let make_expression = make_expression loc in
  Pexp_send
    (make_expression 
       (Pexp_apply
          (make_expression (
              Pexp_ident
                Longident.{txt = Ldot (Lident "Js_unsafe", "unsafe_downgrade"); loc}),
           [(Nolabel, x)])),
     y)

let to_argument_meta { fm_name; fm_description; fm_field_type } = 
  {
    am_name = fm_name;
    am_description = fm_description;
    am_arg_type = fm_field_type;
    am_default_value = None;
  }

module TypeSet = Set.Make (
  struct
    type t = Schema.type_meta
    let compare = Schema.compare_type_meta
  end
  )

let rec extract_variable_types schema acc =
  function
  | [] -> acc
  | h::t -> 
    let ty = lookup_type schema (
        type_name_of_native_type_ref (to_native_type_ref h)
      ) in
    match ty with
    | None ->
      extract_variable_types schema acc t
    | Some x when (TypeSet.exists (fun y -> Schema.compare_type_meta x y == 0) acc) ->
      extract_variable_types schema acc t
    | Some InputObject { iom_name } when String.compare iom_name "_QueryMeta" == 0 -> 
      extract_variable_types schema acc t
    | Some Object { om_name } when String.compare om_name "_QueryMeta" == 0 -> 
      extract_variable_types schema acc t
    | Some Object x ->
      let acc = TypeSet.add (Object x) acc in
      let child_types = List.map (fun x -> x.fm_field_type) x.om_fields in
      extract_variable_types schema (extract_variable_types schema acc child_types) t
    | Some InputObject x ->
      let acc = TypeSet.add (InputObject x) acc in
      let child_types = List.map (fun x -> x.am_arg_type) x.iom_input_fields in
      extract_variable_types schema (extract_variable_types schema acc child_types) t
    | Some Interface x -> 
      let acc = TypeSet.add (Interface x) acc in
      extract_variable_types schema acc t
    | Some Union x -> 
      let acc = TypeSet.add (Union x) acc in
      extract_variable_types schema acc t
    | Some x ->
      let acc = TypeSet.add x acc in
      extract_variable_types schema acc t

let make_value_binding loc pattern expression = {
  pvb_pat = pattern;
  pvb_expr = expression;
  pvb_attributes = [];
  pvb_loc = loc;
}

let make_function loc ~from:name expression =
  let make_pattern = make_pattern loc in
  make_expression loc (Pexp_fun (Nolabel, None, make_pattern (Ppat_var {loc; txt = name; }), expression))

let function_name_string x = "json_of_" ^ Schema.extract_name_from_type_meta x

let rec parser_for_type schema loc type_ref = 
  let make_expression = make_expression loc in
  let apply = apply loc in
  match type_ref with
  | Ntr_list x ->
    let child_parser = (parser_for_type schema loc x) in
    begin match child_parser with
      | Some child_parser ->
        Some (
          make_expression @@
          apply "json_of_array" (
            child_parser
          ))
      | None -> raise @@ Invalid_argument "ABC"
    end
  | Ntr_nullable x ->
    let child_parser = (parser_for_type schema loc x) in
    begin match child_parser with
      | Some child_parser ->
        Some (
          make_expression @@
          apply "json_of_optional" child_parser
        )
      | None -> raise @@ Invalid_argument ""
    end
  | Ntr_named type_name ->
    let type_ = lookup_type schema type_name in
    match type_ with
    | None -> raise @@ Invalid_argument ("Inconsistent schema, type named " ^ type_name ^ " cannot be found")
    | Some InputObject { iom_name } when String.compare iom_name "_QueryMeta" == 0 -> 
      None
    | Some Object { om_name } when String.compare om_name "_QueryMeta" == 0 -> 
      None
    | Some type_ ->
      Some (
        make_expression @@
        make_expression_from_string loc (function_name_string type_)
      )

let rec list_of_fields schema loc expr fields =
  let make_expression = make_expression loc in
  match fields with
  | [] -> make_expression (Pexp_construct ({txt = Longident.Lident "[]"; loc}, None))
  | {am_name; am_arg_type}::t ->
    let type_ref = (to_native_type_ref am_arg_type) in
    match (parser_for_type schema loc type_ref) with
    | None -> list_of_fields schema loc expr t
    | Some parser_ ->
      make_expression (
        Pexp_construct (
          {loc; txt = Longident.Lident "::"},
          Some (
            make_expression ( 
              Pexp_tuple [
                make_expression (
                  Pexp_tuple [
                    make_expression (Pexp_constant (Pconst_string (am_name, None)));
                    make_expression @@ Pexp_apply (
                      parser_,
                      [(Nolabel, make_expression @@ apply_js loc expr am_name)]
                    )
                  ]
                );
                list_of_fields schema loc expr t
              ]
            ))))

let generate_encoder schema loc x =
  let make_value_binding = make_value_binding loc in
  let make_pattern = make_pattern loc in
  let make_value_expression = make_value_expression loc in
  let make_function = make_function loc in
  let make_expression = make_expression loc in
  let make_expression_from_string = make_expression_from_string loc in
  let apply n e = make_expression (apply loc n e) in
  let function_name = make_pattern (Ppat_var {loc; txt = function_name_string x; }) in
  let body = match x with
    | Scalar { sm_name = "String" }
    | Scalar { sm_name = "ID" } -> 
      apply "Js.Json.string" 
    | Scalar { sm_name = "Float" }  ->
      apply "Js.Json.number" 
    | Scalar { sm_name = "Int" }  ->
      fun value ->
        apply "Js.Json.number" (
          apply "float_of_int" value
        )
    | Scalar { sm_name = "Boolean" } ->
      apply "Js.Json.boolean" 
    | Scalar _ ->
      fun x -> x
    | Enum { em_values } ->
      fun expr -> make_expression (
          Pexp_match (
            expr,
            List.map (
              fun { evm_name } -> {
                  pc_rhs = make_expression (
                      Pexp_apply (
                        {
                          pexp_desc = make_expression_from_string "Js.Json.string";
                          pexp_loc = loc; pexp_attributes = []
                        },
                        [
                          (Nolabel, {
                              pexp_desc = Pexp_constant (Pconst_string(evm_name, None));
                              pexp_loc = loc; pexp_attributes = [];
                            });
                        ]
                      )
                    );
                  pc_guard = None;
                  pc_lhs = {
                    ppat_desc =  Ppat_variant (evm_name, None);  
                    ppat_loc = loc; ppat_attributes = [] 
                  }
                }
            ) em_values;
          )
        )
    | Object { om_fields } -> 
      fun expr ->
        apply "Js.Json.object_" (
          apply "Js.Dict.fromList" (list_of_fields schema loc expr (List.map to_argument_meta om_fields))
        )
    | InputObject { iom_input_fields } -> 
      fun expr ->
        apply "Js.Json.object_" (
          apply "Js.Dict.fromList" (list_of_fields schema loc expr iom_input_fields)
        )
    | Interface _ -> raise @@ Invalid_argument "Unsupported variable type: Interface"
    | Union _ -> raise @@ Invalid_argument "Unsupported variable type: Union"
  in
  make_value_binding function_name (make_function ~from:"value" (body (make_value_expression "value"))) 

let make_encoder_function_type loc input_type =
  let make_type = make_type loc in
  let json_t = make_type (
      Ptyp_constr
        ({ txt = Longident.parse "Js.Json.t"; loc },
         [])) in
  make_type (
      Ptyp_poly (
        ["a"], 
        make_type (
          Ptyp_arrow (
            Nolabel,
            make_type (
              Ptyp_arrow (
                Nolabel, make_type (Ptyp_var "a"), json_t)
            ),
            make_type (
              Ptyp_arrow (
                Nolabel,
                make_type (
                  Ptyp_constr (
                    {loc; txt = Longident.Lident input_type},
                    [make_type (Ptyp_var "a")]
                  )
                ),
                json_t))))))

let optional_encoder loc =
  let make_pattern = make_pattern loc in
  let apply = apply loc in
  let make_value_expression = make_value_expression loc in
  let make_expression_from_string = make_expression_from_string loc in
  let make_expression = make_expression loc in
  let make_value_binding = make_value_binding loc in
  let make_function = make_function loc in
  let function_type = make_encoder_function_type loc "option" in
  let function_name = make_pattern (
      Ppat_constraint (
        make_pattern (Ppat_var {loc; txt = "json_of_optional"; }), function_type
      )
    ) in
  make_value_binding function_name (
    make_function ~from:"encoder" (
      make_function ~from:"value" (
        make_expression (
          Pexp_match (
            make_value_expression "value",
            [{ 
              pc_lhs = make_pattern (Ppat_construct ({txt = Longident.Lident "None"; loc = loc}, None));
              pc_guard = None;
              pc_rhs = make_expression @@ make_expression_from_string "Js.Json.null";
            }; {
               pc_lhs = make_pattern (
                   Ppat_construct (
                     {txt = Longident.Lident "Some"; loc = loc}, 
                     Some (make_pattern (Ppat_var {txt = "value"; loc})));
                 );
               pc_guard = None;
               pc_rhs = make_expression @@ apply "encoder" (make_value_expression "value")
             }]
          )))))

let array_encoder loc =
  let make_pattern = make_pattern loc in
  let apply_multi = apply_multi loc in
  let apply = apply loc in
  let make_value_expression = make_value_expression loc in
  let make_expression = make_expression loc in
  let make_value_binding = make_value_binding loc in
  let make_function = make_function loc in
  let function_type = make_encoder_function_type loc "array" in
  let function_name = make_pattern (
      Ppat_constraint (
        make_pattern (Ppat_var {loc; txt = "json_of_array"; }), function_type
      )
    ) in
  make_value_binding function_name (
    make_function ~from:"encoder" (
      make_function ~from:"value" (
        make_expression (
          apply "Js.Json.array" (
            make_expression (
              apply_multi "Array.map"
                [
                  make_value_expression "encoder";
                  make_value_expression "value"
                ]
            ))))))

let generate_encoders schema loc = 
  function
  | [Operation { 
      item = { o_variable_definitions = Some { item } }
    }] -> List.map (fun (_, {vd_type = variable_type}) -> 
      to_schema_type_ref variable_type.item
    ) item 
          |> extract_variable_types schema TypeSet.empty
          |> (fun types -> TypeSet.fold (fun element t -> (generate_encoder schema loc element)::t) types [])
          |> (fun encoders -> (optional_encoder loc)::(array_encoder loc)::encoders)
  | [Operation { item = { o_variable_definitions = None }}] -> []
  | _ -> raise @@ Unimplemented "variables on other than singular queries/mutations" 

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
        | (name, def) :: tl -> 
          let parser_ = (
            parser_for_type schema (map_loc name.span) (
              to_native_type_ref (to_schema_type_ref def.vd_type.item)
            )
          ) in 
          begin match parser_ with
            | None -> make_body tl
            | Some parser_ ->
              Pexp_construct (
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
                              pexp_desc = Pexp_apply (
                                  parser_,
                                  [(Nolabel, make_value_expression (map_loc name.span) name.item)]
                                )
                            ;                         


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
          end
        | [] -> Pexp_construct ({ txt = Longident.Lident "[]"; loc = map_loc span}, None)
      in
      let loc = map_loc span in
      let make_expression_from_string = make_expression_from_string loc in
      let variable_ctor_body = 
        Pexp_apply (
          {pexp_desc = make_expression_from_string "Js.Json.object_";
           pexp_loc = loc; pexp_attributes = []},
          [(Nolabel, {pexp_desc = Pexp_apply ({
               pexp_desc = make_expression_from_string "Js.Dict.fromList";
               pexp_loc = loc; pexp_attributes = []}
               ,
               [(Nolabel, {pexp_desc = (make_body item); pexp_loc = loc; pexp_attributes = []})]);
              pexp_loc = loc; pexp_attributes = []})]) in
      make_labelled_function item (make_make_triple loc variable_ctor_body )
    end
  | [Operation { item = { o_variable_definitions = None }; span }] -> begin
      let loc = map_loc span in
      let make_expression_from_string = make_expression_from_string loc in
      Pexp_fun (
        Nolabel,
        None,
        {ppat_desc = Ppat_construct ({ txt = Longident.Lident "()"; loc = map_loc span }, None );
         ppat_loc = map_loc span; ppat_attributes = []},
        { pexp_desc = make_make_triple loc (make_expression_from_string "Js.Json.null");
          pexp_loc = map_loc span; pexp_attributes = []; })
    end
  | _ -> raise @@ Unimplemented "variables on other than singular queries/mutations"
