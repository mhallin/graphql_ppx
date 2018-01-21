open Ast
open Source_pos
open Schema

open Ast_402
open Parsetree
open Asttypes

open Type_utils
open Generator_utils

exception Unimplemented of string

let has_directive name directives =
  List.exists (fun { item = { d_name = { item } } } -> item = name) directives

let rec unify_type as_record map_loc span ty schema (selection_set: selection list spanning option) =
  let loc = map_loc span in
  match ty with
  | Ntr_nullable t ->
    [%expr match Js.Json.decodeNull value with
      | None -> Some [%e unify_type as_record map_loc span t schema selection_set]
      | Some _ -> None
    ] [@metaloc loc]
  | Ntr_list t ->
    [%expr match Js.Json.decodeArray value with
      | None -> raise Graphql_error
      | Some value -> Js.Array.map (fun value -> [%e unify_type as_record map_loc span t schema selection_set]) value
    ] [@metaloc loc]
  | Ntr_named n -> match lookup_type schema n with
    | None -> raise_error map_loc span ("Could not find type " ^ n)
    | Some Scalar { sm_name = "ID" } 
    | Some Scalar { sm_name = "String" } ->
      [%expr match Js.Json.decodeString value with
        | None -> raise Graphql_error
        | Some value -> (value : string)
      ] [@metaloc loc]
    | Some Scalar { sm_name = "Int" } ->
      [%expr match Js.Json.decodeNumber value with
        | None -> raise Graphql_error
        | Some value -> int_of_float value
      ] [@metaloc loc]
    | Some Scalar { sm_name = "Float" } ->
      [%expr match Js.Json.decodeNumber value with
        | None -> raise Graphql_error
        | Some value -> value
      ] [@metaloc loc]
    | Some Scalar { sm_name = "Boolean" } ->
      [%expr match Js.Json.decodeBoolean value with
        | None -> raise Graphql_error
        | Some value -> value
      ] [@metaloc loc]
    | Some Scalar _ -> 
      Ast_helper.(Exp.ident ~loc:loc {txt=Longident.Lident "value"; loc = loc})
    | Some ((Object o) as ty) ->
      unify_selection_set as_record map_loc span schema ty selection_set
    | Some Enum { em_name; em_values } ->
      let enum_ty = Ast_helper.(
          Typ.variant ~loc:loc
            (List.map (fun { evm_name } -> Rtag (evm_name, [], true, [])) em_values)
            Closed None)
      in
      let enum_vals = Ast_helper.(
          Exp.match_ ~loc:loc [%expr value]
            (List.concat [
                List.map (fun { evm_name } ->
                    Exp.case
                      (Pat.constant ~loc:loc (Const_string (evm_name, None)))
                      (Exp.variant ~loc:loc evm_name None)) em_values;
                [Exp.case (Pat.any ()) [%expr raise Graphql_error]]]))
      in
      [%expr match Js.Json.decodeString value with
        | None -> raise Graphql_error
        | Some value -> ([%e enum_vals] : [%t enum_ty])
      ] [@metaloc loc]
    | Some ((Interface o) as ty) ->
      unify_selection_set as_record map_loc span schema ty selection_set
    | Some InputObject obj -> raise_error map_loc span "Can't have fields on input objects"
    | Some Union um -> unify_union map_loc span schema um selection_set

and unify_union map_loc span schema union_meta selection_set =
  match selection_set with
  | None -> raise_error map_loc span "Union types must have subselections"
  | Some selection_set ->
    let unwrap_type_conds selection = match selection with
      | Field { span } | FragmentSpread { span } -> raise_error map_loc span "Only inline fragments can appear on unions"
      | InlineFragment { item = { if_type_condition = None }; span } ->
        raise_error map_loc span "Inline fragments must have a type condition"
      | InlineFragment frag -> frag
    in
    let type_cond_name { item = { if_type_condition = Some { item }}} = item in
    let generate_case { item = { if_type_condition = Some if_type_condition; if_selection_set}; span } =
      let type_cond_loc = map_loc if_type_condition.span in
      let type_cond_pat = Ast_helper.Pat.constant ~loc:type_cond_loc (Const_string (if_type_condition.item, None)) in
      let type_cond_ty = match lookup_type schema if_type_condition.item with
        | None -> raise_error map_loc if_type_condition.span "Could not find type"
        | Some ty -> ty
      in
      let result_decoder = unify_selection_set false map_loc if_selection_set.span schema type_cond_ty (Some if_selection_set) in
      let selection_set_loc = map_loc if_selection_set.span in
      let result_variant = Ast_helper.Exp.variant ~loc:selection_set_loc if_type_condition.item (Some result_decoder) in
      Ast_helper.Exp.case
        ([%pat? Some [%p type_cond_pat]] [@metaloc map_loc span])
        (result_variant [@metaloc map_loc span])
    in
    let loc = map_loc span in
    let fragments = List.map unwrap_type_conds selection_set.item in
    let covered_cases = List.map type_cond_name fragments |> List.sort compare in
    let possible_cases = List.sort compare union_meta.um_of_types in
    let typename_decode = [%expr Js.Json.decodeString (Js.Dict.unsafeGet unionValue "__typename")] [@metaloc loc] in
    let none_case = Ast_helper.Exp.case [%pat? None ] [%expr raise Graphql_error] in
    let fail_case = if covered_cases = possible_cases then
        Ast_helper.Exp.case [%pat? Some _ ] [%expr raise Graphql_error]
      else 
        Ast_helper.Exp.case [%pat? Some _ ] [%expr `Nonexhaustive ]
    in
    let union_ty = Ast_helper.(
      Typ.variant ~loc:loc
        (let case_variants = List.map(fun evm_name -> Rtag (evm_name, [], false, [{ ptyp_desc = Ptyp_any; ptyp_attributes = []; ptyp_loc = loc }])) covered_cases
        in if covered_cases = possible_cases
          then case_variants
          else Rtag ("Nonexhaustive", [], true, []) :: case_variants)
        Closed None)
    in
    let decoder = Ast_helper.Exp.match_ ~loc typename_decode (List.append (List.map generate_case fragments) [fail_case; none_case]) in
    [%expr match Js.Json.decodeObject value with
      | None -> raise Graphql_error
      | Some unionValue -> ([%e decoder]: [%t union_ty])] [@metaloc loc]


and unify_variant map_loc span ty schema selection_set =
  let loc = map_loc span in
  let selection_name = function
    | Field { item } -> String.capitalize (some_or item.fd_alias item.fd_name).item
    | FragmentSpread { span } -> raise_error map_loc span "Variant selections can only contain fields"
    | InlineFragment { span } -> raise_error map_loc span "Variant selections can only contain fields"
  in
  let rec match_loop ty selection_set = match selection_set with
    | [] -> [%expr raise Graphql_error] [@metaloc loc]
    | Field { item; span } :: tl -> begin
        match lookup_field ty item.fd_name.item with
        | None -> raise_error map_loc span ("Unknown field on type " ^ type_name ty)
        | Some field_meta ->
          let key = (some_or item.fd_alias item.fd_name).item in
          let inner_type = match (to_native_type_ref field_meta.fm_field_type) with
            | Ntr_list _ | Ntr_named _ -> raise_error map_loc span "Variant field must only contain nullable fields"
            | Ntr_nullable i -> i in
          [%expr
            let temp = Js.Dict.unsafeGet value [%e Ast_helper.(Exp.constant ~loc:loc (Const_string (key, None)))] in
            match Js.Json.decodeNull temp with
            | None -> let value = temp in 
              [%e Ast_helper.(Exp.variant ~loc:loc 
                                (String.capitalize key)
                                (Some (unify_type false map_loc span inner_type schema item.fd_selection_set)))]
            | Some _ -> [%e match_loop ty tl]] [@metaloc loc]
      end
    | FragmentSpread { span } :: _ -> raise_error map_loc span "Variant selections can only contain fields"
    | InlineFragment { span } :: _ -> raise_error map_loc span "Variant selections can only contain fields"
  in
  match ty with
  | Ntr_nullable t -> 
    [%expr match Js.Json.decodeNull value with
      | None -> None
      | Some value -> Some [%e unify_variant map_loc span t schema selection_set]
    ] [@metaloc loc]
  | Ntr_list t ->
    [%expr match Js.Json.decodeArray value with
      | None -> raise Graphql_error
      | Some value -> Js.Array.map (fun value -> [%e unify_variant map_loc span t schema selection_set]) value
    ] [@metaloc loc]
  | Ntr_named n -> match lookup_type schema n with
    | None -> raise_error map_loc span ("Could not find type " ^ n)
    | Some Scalar _ -> raise_error map_loc span "Variant fields can only be applied to object types"
    | Some Enum _ -> raise_error map_loc span "Variant fields can only be applied to object types"
    | Some Interface _ -> raise_error map_loc span "Variant fields can only be applied to object types"
    | Some Union _ -> raise_error map_loc span "Variant fields can only be applied to object types"
    | Some InputObject _ -> raise_error map_loc span "Variant fields can only be applied to object types"
    | Some ((Object _) as ty) ->
      match selection_set with
      | None -> raise_error map_loc span "Variant fields need a selection set"
      | Some { item } ->
        let matcher = match_loop ty item in
        let variant_type = Ast_helper.(
          Typ.variant ~loc:loc
            (List.map(fun s -> Rtag (selection_name s, [], false, [{ ptyp_desc = Ptyp_any; ptyp_attributes = []; ptyp_loc = loc }])) item)
            Closed None) in
        [%expr match Js.Json.decodeObject value with
          | None -> raise Graphql_error
          | Some value -> ([%e matcher]: [%t variant_type])] [@metaloc loc]

and unify_field map_loc field_span ty schema =
  let ast_field = field_span.item in
  let field_meta = lookup_field ty ast_field.fd_name.item in
  let key = (some_or ast_field.fd_alias ast_field.fd_name).item in
  let loc = map_loc field_span.span in
  let is_variant = has_directive "bsVariant" ast_field.fd_directives in
  let is_record = has_directive "bsRecord" ast_field.fd_directives in
  let has_skip = (has_directive "skip" ast_field.fd_directives) 
                 || (has_directive "include" ast_field.fd_directives) in
  let sub_unifier =
    if is_variant then unify_variant 
    else (unify_type is_record)
  in
  let field_key = { txt = Longident.Lident key; loc = loc } in
  let parser_expr = match field_meta with
    | None -> raise_error map_loc field_span.span ("Unknown field on type " ^ type_name ty)
    | Some field_meta ->
      let field_ty = to_native_type_ref field_meta.fm_field_type in
      let field_ty_is_nullable = is_nullable field_ty in
      let sub_unifier = sub_unifier map_loc field_span.span field_ty schema ast_field.fd_selection_set in
      if has_skip then
        [%expr
          match Js.Dict.get value [%e Ast_helper.Exp.constant ~loc:loc (Const_string (key, None))] with
          | None -> None
          | Some value -> [%e 
            if field_ty_is_nullable then sub_unifier
            else [%expr Some [%e sub_unifier]] [@metaloc loc]
          ]
        ] [@metaloc loc]
      else
        [%expr
          let value = Js.Dict.unsafeGet value [%e Ast_helper.Exp.constant ~loc:loc (Const_string (key, None))]
          in [%e sub_unifier]
        ] [@metaloc loc]
  in
  match List.filter (fun { item = { d_name = { item } } } -> item = "bsDecoder") ast_field.fd_directives with
  | [] -> (field_key, parser_expr)
  | { item = { d_arguments = Some { item = [({ item = "fn" }, { item = Iv_string fn_name; span})] } }} :: _ -> 
    let loc = map_loc span in
    (field_key,
     Ast_helper.Exp.apply ~loc
       (Ast_helper.Exp.ident ~loc {txt = Longident.parse fn_name; loc = loc})
       [("", parser_expr)])
  | { item = { d_arguments = None }; span }:: _ -> raise_error map_loc span "bsDecoder must be given 'fn' argument"
  | { item = { d_arguments = Some _ }; span }:: _ -> raise_error map_loc span "bsDecoder must be given 'fn' argument"

and unify_selection map_loc schema ty selection = match selection with
  | Field field_span -> unify_field map_loc field_span ty schema
  | FragmentSpread _ -> raise @@ Unimplemented "fragment spreads"
  | InlineFragment _ -> raise @@ Unimplemented "inline fragments"

and unify_selection_set as_record map_loc span schema ty selection_set = match selection_set with
  | None -> raise_error map_loc span "Must select subfields on objects"
  | Some { item } when as_record -> let loc = map_loc span in
    [%expr match Js.Json.decodeObject value with
      | None -> raise Graphql_error
      | Some value -> [%e Ast_helper.Exp.record ~loc (List.map (unify_selection map_loc schema ty) item) None]
    ] [@metaloc loc]
  | Some { item } -> let loc = map_loc span in
    let fields = List.map (unify_selection map_loc schema ty) item in
    let ctor_result_type = (List.mapi 
                              (fun i (key, _) -> (Longident.last key.txt, [], Ast_helper.Typ.var ~loc ("a" ^ (string_of_int i)))) 
                              fields)
    in
    let rec make_obj_constructor_fn i = function
      | [] -> Ast_helper.Typ.arrow ~loc "" (Ast_helper.Typ.constr { txt = Longident.Lident "unit"; loc = loc} [])
                (Ast_helper.Typ.constr ~loc { txt = Longident.parse "Js.t"; loc = loc} [
                    (Ast_helper.Typ.object_ ~loc
                       ctor_result_type
                       Closed)
                  ])
      | (key, _) :: next -> Ast_helper.Typ.arrow ~loc (Longident.last key.txt) (Ast_helper.Typ.var ~loc ("a" ^ (string_of_int i)))
                              (make_obj_constructor_fn (i+1) next)
    in
    [%expr match Js.Json.decodeObject value with
      | None -> raise Graphql_error
      | Some value -> 
        [%e
          Ast_helper.Exp.letmodule ~loc {txt="GQL"; loc=loc} (Ast_helper.Mod.structure ~loc [
              Ast_helper.Str.primitive ~loc {
                pval_name = {txt = "make_obj"; loc = loc};
                pval_type = make_obj_constructor_fn 0 fields;
                pval_prim = [""];
                pval_attributes = [({txt = "bs.obj"; loc = loc}, PStr [])];
                pval_loc = loc;
              }
            ])
            (Ast_helper.Exp.apply ~loc (Ast_helper.Exp.ident ~loc { txt = Longident.parse "GQL.make_obj"; loc = loc})
               (List.append
                  (List.map (fun (key, parser) -> (Longident.last key.txt, parser)) fields)
                  [("", Ast_helper.Exp.construct ~loc { txt = Longident.Lident "()"; loc = loc} None)]
               ))
        ]
    ] [@metaloc loc]


let unify_document_schema map_loc schema document =
  match document with
  | [Operation { item = { o_type = Query; o_selection_set }; span } ] ->
    unify_selection_set false map_loc span schema (query_type schema) (Some o_selection_set)
  | [Operation { item = { o_type = Mutation; o_selection_set }; span } ] -> begin match mutation_type schema with
      | Some mutation_type -> 
        unify_selection_set false map_loc span schema mutation_type (Some o_selection_set)
      | None ->
        raise_error map_loc span "This schema does not contain any mutations"
    end
  | _ -> raise @@ Unimplemented "unification with other than singular queries"
