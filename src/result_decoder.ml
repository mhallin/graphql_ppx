open Graphql_ast
open Source_pos
open Schema

open Ast_402
open Parsetree
open Asttypes

open Type_utils
open Generator_utils
open Result_structure

exception Unimplemented of string

let make_error error_marker map_loc span message =
  let () = error_marker.has_error <- true in
  Res_error (map_loc span, message)

let has_directive name directives =
  List.exists (fun { item = { d_name = { item } } } -> item = name) directives

let string_of_longident longident =
  Longident.flatten longident |> String.concat "."

let rec unify_type error_marker as_record config span ty (selection_set: selection list spanning option) =
  match ty with
  | Ntr_nullable t ->
    Res_nullable (config.map_loc span, unify_type error_marker as_record config span t selection_set)
  | Ntr_list t ->
    Res_array (config.map_loc span, unify_type error_marker as_record config span t selection_set)
  | Ntr_named n -> match lookup_type config.schema n with
    | None -> raise_error config.map_loc span ("Could not find type " ^ n)
    | Some Scalar { sm_name = "ID" } 
    | Some Scalar { sm_name = "String" } -> Res_string (config.map_loc span)
    | Some Scalar { sm_name = "Int" } -> Res_int (config.map_loc span)
    | Some Scalar { sm_name = "Float" } -> Res_float (config.map_loc span)
    | Some Scalar { sm_name = "Boolean" } -> Res_boolean (config.map_loc span)
    | Some Scalar _ -> Res_raw_scalar (config.map_loc span)
    | Some ((Object o) as ty) ->
      unify_selection_set error_marker as_record config span ty selection_set
    | Some Enum enum_meta -> Res_poly_enum (config.map_loc span, enum_meta)
    | Some ((Interface o) as ty) ->
      unify_selection_set error_marker as_record config span ty selection_set
    | Some InputObject obj -> make_error error_marker config.map_loc span "Can't have fields on input objects"
    | Some Union um -> unify_union error_marker config span um selection_set

and unify_union error_marker config span union_meta selection_set =
  match selection_set with
  | None -> make_error error_marker config.map_loc span "Union types must have subselections"
  | Some selection_set ->
    let unwrap_type_conds selection = match selection with
      | Field { span } | FragmentSpread { span } -> raise_error config.map_loc span "Only inline fragments can appear on unions"
      | InlineFragment { item = { if_type_condition = None }; span } ->
        raise_error config.map_loc span "Inline fragments must have a type condition"
      | InlineFragment frag -> frag
    in
    let type_cond_name { item = { if_type_condition = Some { item }}} = item in
    let generate_case { item = { if_type_condition = Some if_type_condition; if_selection_set}; span } =
      let type_cond_ty = match lookup_type config.schema if_type_condition.item with
        | None -> raise_error config.map_loc if_type_condition.span "Could not find type"
        | Some ty -> ty
      in
      let result_decoder = unify_selection_set error_marker false config if_selection_set.span type_cond_ty (Some if_selection_set) in
      (if_type_condition.item, result_decoder) in
    let fragments = List.map unwrap_type_conds selection_set.item in
    let covered_cases = List.map type_cond_name fragments |> List.sort compare in
    let possible_cases = List.sort compare union_meta.um_of_types in
    Res_poly_variant_union (
      config.map_loc span,
      union_meta.um_name,
      (List.map generate_case fragments),
      if covered_cases = possible_cases then Exhaustive else Nonexhaustive
    )

and unify_variant error_marker config span ty selection_set =
  match ty with
  | Ntr_nullable t -> Res_nullable (config.map_loc span, unify_variant error_marker config span t selection_set)
  | Ntr_list t -> Res_array (config.map_loc span, unify_variant error_marker config span t selection_set)
  | Ntr_named n -> match lookup_type config.schema n with
    | None -> make_error error_marker config.map_loc span ("Could not find type " ^ n)
    | Some Scalar _ -> make_error error_marker config.map_loc span "Variant fields can only be applied to object types"
    | Some Enum _ -> make_error error_marker config.map_loc span "Variant fields can only be applied to object types"
    | Some Interface _ -> make_error error_marker config.map_loc span "Variant fields can only be applied to object types"
    | Some Union _ -> make_error error_marker config.map_loc span "Variant fields can only be applied to object types"
    | Some InputObject _ -> make_error error_marker config.map_loc span "Variant fields can only be applied to object types"
    | Some ((Object _) as ty) ->
      match selection_set with
      | None -> make_error error_marker config.map_loc span "Variant fields need a selection set"
      | Some { item } ->
        let fields = item |> List.map (fun selection -> match selection with
            | Field { item } -> begin
                match lookup_field ty item.fd_name.item with
                | None -> raise_error config.map_loc span ("Unknown field on type " ^ type_name ty)
                | Some field_meta ->
                  let key = (some_or item.fd_alias item.fd_name).item in
                  let inner_type = match (to_native_type_ref field_meta.fm_field_type) with
                    | Ntr_list _ | Ntr_named _ -> raise_error config.map_loc span "Variant field must only contain nullable fields"
                    | Ntr_nullable i -> i in
                  (
                    key,
                    unify_type error_marker false config span inner_type item.fd_selection_set
                  )
              end
            | FragmentSpread { span } -> raise_error config.map_loc span "Variant selections can only contain fields"
            | InlineFragment { span } -> raise_error config.map_loc span "Variant selections can only contain fields"
          ) in
        Res_poly_variant_selection_set (config.map_loc span, n, fields)

and unify_field error_marker config field_span ty =
  let ast_field = field_span.item in
  let field_meta = lookup_field ty ast_field.fd_name.item in
  let key = (some_or ast_field.fd_alias ast_field.fd_name).item in
  let is_variant = has_directive "bsVariant" ast_field.fd_directives in
  let is_record = has_directive "bsRecord" ast_field.fd_directives in
  let has_skip = (has_directive "skip" ast_field.fd_directives) 
                 || (has_directive "include" ast_field.fd_directives) in
  let sub_unifier =
    if is_variant then unify_variant error_marker
    else unify_type error_marker is_record
  in
  let parser_expr = match field_meta with
    | None -> make_error error_marker config.map_loc field_span.span ("Unknown field on type " ^ type_name ty)
    | Some field_meta ->
      let field_ty = to_native_type_ref field_meta.fm_field_type in
      let sub_unifier = sub_unifier config field_span.span field_ty ast_field.fd_selection_set in
      if has_skip && not (is_nullable field_ty) then
        Res_nullable (config.map_loc field_span.span, sub_unifier)
      else
        sub_unifier
  in
  match List.filter (fun { item = { d_name = { item } } } -> item = "bsDecoder") ast_field.fd_directives with
  | [] -> (key, parser_expr)
  | { item = { d_arguments = Some { item = [({ item = "fn" }, { item = Iv_string fn_name; span})] } }} :: _ -> 
    (key, Res_custom_decoder (config.map_loc span, fn_name, parser_expr))
  | { item = { d_arguments = None }; span }:: _ -> (key, make_error error_marker config.map_loc span "bsDecoder must be given 'fn' argument")
  | { item = { d_arguments = Some _ }; span }:: _ -> (key, make_error error_marker config.map_loc span "bsDecoder must be given 'fn' argument")

and unify_selection error_marker config ty selection = match selection with
  | Field field_span -> unify_field error_marker config field_span ty
  | FragmentSpread _ -> raise @@ Unimplemented "fragment spreads"
  | InlineFragment _ -> raise @@ Unimplemented "inline fragments"

and unify_selection_set error_marker as_record config span ty selection_set = match selection_set with
  | None -> make_error error_marker config.map_loc span "Must select subfields on objects"
  | Some { item = [ FragmentSpread { item } ] } ->
    if as_record then
      make_error error_marker config.map_loc span "@bsRecord can not be used with fragment spreads, place @bsRecord on the fragment definition instead"
    else
      Res_solo_fragment_spread (config.map_loc span, item.fs_name.item)
  | Some { item } when as_record ->
    Res_record (config.map_loc span, type_name ty, List.map (unify_selection error_marker config ty) item)
  | Some { item } ->
    Res_object (config.map_loc span, type_name ty, List.map (unify_selection error_marker config ty) item)

let unify_operation error_marker config = function
  | { item = { o_type = Query; o_selection_set; o_variable_definitions }; span }
    -> unify_selection_set error_marker false config span (query_type config.schema) (Some o_selection_set)
  | { item = { o_type = Mutation; o_selection_set; o_variable_definitions }; span } 
    -> begin match mutation_type config.schema with
        | Some mutation_type -> 
          unify_selection_set error_marker false config span mutation_type (Some o_selection_set)
        | None ->
          make_error error_marker config.map_loc span "This schema does not contain any mutations"
      end
  | { item = { o_type = Subscription; o_selection_set; o_variable_definitions }; span }
    -> begin match subscription_type config.schema with
        | Some subscription_type ->
          unify_selection_set error_marker false config span subscription_type (Some o_selection_set)
        | None->
          make_error error_marker config.map_loc span "This schema does not contain any subscriptions"
      end

let rec unify_document_schema config document =
  let error_marker = { Generator_utils.has_error = false } in
  match document with
  | [Operation ({ item = { o_variable_definitions } } as op) ] ->
    let structure = unify_operation error_marker config op in
    [Mod_default_operation (o_variable_definitions, error_marker.has_error, op, structure)]
  | Fragment ({ item = { fg_name; fg_selection_set; fg_type_condition; fg_directives }; span } as fg) :: rest -> begin
      let is_record = has_directive "bsRecord" fg_directives in
      match Schema.lookup_type config.schema fg_type_condition.item with
      | None -> Mod_fragment (
          fg_name.item, [], true,
          fg, make_error error_marker config.map_loc span (Printf.sprintf "Unknown type \"%s\"" fg_type_condition.item)
        )
      | Some ty -> 
        let structure = unify_selection_set error_marker is_record config span ty (Some fg_selection_set) in
        Mod_fragment (fg_name.item, [], error_marker.has_error, fg, structure)
    end :: unify_document_schema config rest
  | [] -> []
  | _ -> raise @@ Unimplemented "unification with other than singular queries"
