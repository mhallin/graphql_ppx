open Graphql_ppx_base
open Result_structure
open Schema

open Ppxlib
open Asttypes
open Parsetree

open Generator_utils
open Output_native_utils

let const_str_expr s = Ast_helper.(Exp.constant (Pconst_string (s, None)))

let make_error_raiser loc message =
  if Ppx_config.verbose_error_handling () then
    [%expr raise (Failure ("graphql_ppx: " ^ [%e message]))]
  else
    [%expr raise (Failure ("Unexpected GraphQL query response"))]

let string_decoder loc =
  [%expr match value with
    | `String value -> value
    | _ -> [%e make_error_raiser loc [%expr "Expected string, got " ^ (Yojson.Basic.to_string value)]]] [@metaloc loc]

let id_decoder = string_decoder

let float_decoder loc =
  [%expr match value with
    | `Float value -> value
    | `Int value -> float_of_int value
    | _ -> [%e make_error_raiser loc [%expr "Expected float, got " ^ (Yojson.Basic.to_string value)]]] [@metaloc loc]

let int_decoder loc =
  [%expr match value with
    | `Int value -> value
    | _ -> [%e make_error_raiser loc [%expr "Expected int, got " ^ (Yojson.Basic.to_string value)]]] [@metaloc loc]

let boolean_decoder loc =
  [%expr match value with
    | `Bool value -> value
    | _ -> [%e make_error_raiser loc [%expr "Expected boolean, got " ^ (Yojson.Basic.to_string value)]]] [@metaloc loc]

let generate_poly_enum_decoder loc enum_meta =
  let enum_match_arms = Ast_helper.(
      List.map
        (fun {evm_name; _} -> Exp.case 
            (Pat.constant (Pconst_string (evm_name, None)))
            (Exp.variant evm_name None))
        enum_meta.em_values) in
  let fallback_arm = Ast_helper.(
      Exp.case
        (Pat.any ())
        (make_error_raiser loc [%expr "Unknown enum variant for " ^ [%e const_str_expr enum_meta.em_name] ^ ": " ^ value])) in
  let match_expr = Ast_helper.(Exp.match_
                                 [%expr value]
                                 (List.concat [enum_match_arms; [fallback_arm]])) in
  let enum_ty = Ast_helper.(
      Typ.variant
        (List.map (fun { evm_name; _ } -> Rtag ({ txt = evm_name; loc }, [], true, [])) enum_meta.em_values)
        Closed None) [@metaloc loc]
  in
  [%expr match value with 
    | `String value -> ([%e match_expr]: [%t enum_ty])
    | _ -> [%e make_error_raiser loc [%expr
        "Expected enum value for " ^
        [%e const_str_expr enum_meta.em_name] ^
        ", got " ^ (Yojson.Basic.to_string value)]]]

let generate_solo_fragment_spread loc name =
  let ident = Ast_helper.Exp.ident { loc; txt = Longident.parse (name ^ ".parse") } in
  [%expr [%e ident] value]

let generate_error loc message =
  let ext = Location.Error.to_extension (Location.Error.createf ~loc "%s" message) in
  [%expr let _value = value in [%e Ast_helper.Exp.extension ~loc ext]]

let rec generate_decoder config = function
  | Res_nullable (loc, inner) -> generate_nullable_decoder config (conv_loc loc) inner
  | Res_array (loc, inner) -> generate_array_decoder config (conv_loc loc) inner
  | Res_id loc -> id_decoder (conv_loc loc)
  | Res_string loc -> string_decoder (conv_loc loc)
  | Res_int loc -> int_decoder (conv_loc loc)
  | Res_float loc -> float_decoder (conv_loc loc)
  | Res_boolean loc -> boolean_decoder (conv_loc loc)
  | Res_raw_scalar loc -> let loc = conv_loc loc in [%expr value]
  | Res_poly_enum (loc, enum_meta) -> generate_poly_enum_decoder (conv_loc loc) enum_meta
  | Res_custom_decoder (loc, ident, inner) -> generate_custom_decoder config (conv_loc loc) ident inner
  | Res_record (loc, name, fields) -> generate_record_decoder config (conv_loc loc) name fields
  | Res_object (loc, name, fields) -> generate_object_decoder config (conv_loc loc) name fields
  | Res_poly_variant_selection_set (loc, name, fields) -> generate_poly_variant_selection_set config (conv_loc loc) name fields
  | Res_poly_variant_union (loc, name, fragments, exhaustive) -> generate_poly_variant_union config (conv_loc loc) name fragments exhaustive
  | Res_poly_variant_interface (loc, name, base, fragments) -> generate_poly_variant_interface config (conv_loc loc) name base fragments
  | Res_solo_fragment_spread (loc, name) -> generate_solo_fragment_spread (conv_loc loc) name
  | Res_error (loc, message) -> generate_error (conv_loc loc) message

and generate_nullable_decoder config loc inner =
  [%expr match value with
    | `Null -> None
    | value -> Some [%e generate_decoder config inner]] [@metaloc loc]

and generate_array_decoder config loc inner =
  [%expr match value with
    | `List value -> List.map (fun value -> [%e generate_decoder config inner]) value |> Array.of_list
    | _ -> [%e make_error_raiser loc [%expr ("Expected array, got " ^ (Yojson.Basic.to_string value))]]] [@metaloc loc]

and generate_custom_decoder config loc ident inner =
  let fn_expr = Ast_helper.(Exp.ident
                              { loc = Location.none; txt = Longident.parse ident }) in
  [%expr [%e fn_expr] ([%e generate_decoder config inner])] [@metaloc loc]


and generate_record_decoder config loc name fields =
  (*
    Given a selection set, this function first generates the resolvers,
    binding the results to individual local variables. It then generates
    a record containing each field bound to the corresponding variable.

    While this might seem a bit convoluted, it lets us wrap the record in
    a [%bs.obj ] extension to generate a JavaScript object without having
    to worry about the record-in-javascript-object problem that BuckleScript has.

    As an example, a selection set like "{ f1 f2 f3 }" will result in
    let (field_f1, field_f2, field_f3) = (
      match Js.Dict.get value "f1" with ... end
      match Js.Dict.get value "f2" with ... end
      match Js.Dict.get value "f3" with ... end
    ) in { f1 = field_f1; f2 = field_f2; f3 = field_f3 }
  *)

  let field_name_tuple_pattern = Ast_helper.(
      fields
      |> filter_map (function
          | Fr_named_field (field, _, _) -> Some (Pat.var { loc; txt = "field_" ^ field })
          | Fr_fragment_spread _ -> None)
      |> Pat.tuple) in

  let field_decoder_tuple = Ast_helper.(
      fields
      |> filter_map (function
          | Fr_named_field (field, loc, inner) -> 
            let loc = conv_loc loc in
            Some [%expr match List.assoc_opt [%e const_str_expr field] value with
              | Some value -> [%e generate_decoder config inner]
              | None -> [%e
                if can_be_absent_as_field inner then
                  [%expr None ]
                else 
                  make_error_raiser loc [%expr
                    "Field " ^ [%e const_str_expr field] ^
                    " on type " ^ [%e const_str_expr name] ^ " is missing"]]] [@metaloc loc]
          | Fr_fragment_spread _ -> None)
      |> Exp.tuple) in

  let record_fields = Ast_helper.(
      fields
      |> List.map (function
          | Fr_named_field (field, loc, _) ->
            let loc = conv_loc loc in
            ({ Location.loc = loc; txt = Longident.Lident field}, 
             Exp.ident ~loc { loc; txt = Longident.Lident ("field_" ^ field) })
          | Fr_fragment_spread (field, loc, name) ->
            let loc = conv_loc loc in
            ({ Location.loc = loc; txt = Longident.Lident field},
             [%expr let value = `Assoc value in [%e generate_solo_fragment_spread loc name]] [@metaloc loc]))) in
  let record = Ast_helper.Exp.record ~loc record_fields None in

  [%expr match value with
    | `Assoc value ->
      let [%p field_name_tuple_pattern] = [%e field_decoder_tuple]
      in [%e record]
    | None -> [%e make_error_raiser loc [%expr
        "Expected object of type " ^
        [%e const_str_expr name] ^
        ", got " ^ (Yojson.Basic.to_string value)]]]

and generate_object_decoder config loc name fields =
  [%expr match value with
    | `Assoc value ->
      [%e
        Ast_helper.(
          Exp.object_ 
            (Cstr.mk 
               (Pat.any ())
               (List.map (function
                    | Fr_named_field (key, _, inner) -> 
                      Cf.method_
                        { txt = key; loc = Location.none }
                        Public
                        (Cfk_concrete (Fresh,
                                       [%expr match List.assoc_opt [%e const_str_expr key] value with
                                         | Some value -> [%e generate_decoder config inner]
                                         | None -> [%e
                                           if can_be_absent_as_field inner then
                                             [%expr None]
                                           else 
                                             make_error_raiser loc [%expr "Field " ^ [%e const_str_expr key] ^ " on type " ^ [%e const_str_expr name] ^ " is missing"]
                                         ]]))
                    | Fr_fragment_spread (key, loc, name) ->
                      let loc = conv_loc loc in
                      Cf.method_
                        { txt = key; loc = Location.none }
                        Public
                        (Cfk_concrete (Fresh, [%expr let value = `Assoc value in [%e generate_solo_fragment_spread loc name]]))
                  ) fields)))
      ]
    | _ -> [%e make_error_raiser loc [%expr "Object is not a value"]]
  ] [@metaloc loc]

and generate_poly_variant_selection_set config loc name fields =
  let rec generator_loop = function
    | (field, inner) :: next ->
      let variant_decoder = Ast_helper.(Exp.variant
                                          (Compat.capitalize_ascii field)
                                          (Some (generate_decoder config inner))) in
      [%expr match List.assoc_opt [%e const_str_expr field] value with
        | None -> [%e make_error_raiser loc [%expr
            "Field " ^ [%e const_str_expr field] ^
            " on type " ^ [%e const_str_expr name] ^ " is missing"]]
        | Some temp -> match temp with
          | `Null -> [%e generator_loop next]
          | _ -> let value = temp in [%e variant_decoder]]
    | [] -> make_error_raiser loc [%expr
              "All fields on variant selection set on type " ^ 
              [%e const_str_expr name] ^
              " were null"] in
  let variant_type = Ast_helper.(
      Typ.variant
        (List.map (fun (name, _) -> Rtag ({ txt = Compat.capitalize_ascii name; loc }, [], false, [{ ptyp_desc = Ptyp_any; ptyp_attributes = []; ptyp_loc = Location.none }])) fields)
        Closed None) in
  [%expr match value with
    | `Assoc value -> ([%e generator_loop fields]: [%t variant_type])
    | _ -> [%e make_error_raiser loc [%expr "Expected type " ^ [%e const_str_expr name] ^ " to be an object"]]] [@metaloc loc]

and generate_poly_variant_interface config loc name base fragments =
  let map_fallback_case (type_name, inner) = Ast_helper.(
      let name_pattern = Pat.any () in
      let variant = Exp.variant type_name (Some (generate_decoder config inner)) in
      Exp.case name_pattern variant
    ) in

  let map_case (type_name, inner) = Ast_helper.(
      let name_pattern = Pat.constant (Pconst_string (type_name, None)) in
      let variant = Exp.variant type_name (Some (generate_decoder config inner)) in
      Exp.case name_pattern variant
    ) in 
  let map_case_ty (name, _) =
    Rtag (name, [], false, [{ ptyp_desc = Ptyp_any; ptyp_attributes = []; ptyp_loc = Location.none }])
  in

  let fragment_cases = List.map map_case fragments in
  let fallback_case = map_fallback_case base in
  let base_name, base_decoder = base in
  let fallback_case_ty = map_case_ty ({ txt = base_name; loc }, base_decoder) in 

  let fragment_case_tys = List.map map_case_ty (fragments |> List.map (fun (key, ty) -> ({ txt = key; loc }, ty))) in
  let interface_ty = Ast_helper.(Typ.variant (fallback_case_ty :: fragment_case_tys) Closed None) in
  let typename_matcher = Ast_helper.(Exp.match_
                                       [%expr typename]
                                       (List.concat [ fragment_cases; [ fallback_case ]])) in
  [%expr
    match value with
    | `Assoc typename_obj -> begin match List.assoc_opt "__typename" typename_obj with
        | None -> [%e make_error_raiser loc [%expr
            "Interface implementation" ^ [%e const_str_expr name] ^
            " is missing the __typename field"]]
        | Some typename -> begin match typename with
            | `String typename -> ([%e typename_matcher]: [%t interface_ty])
            | _ -> [%e make_error_raiser loc [%expr
                "Interface implementation " ^ [%e const_str_expr name] ^
                " has a __typename field that is not a string"]]
          end
      end
    | _ -> [%e make_error_raiser loc
        [%expr "Expected Interface implementation " ^ [%e const_str_expr name] ^ " to be an object, got " ^ (Yojson.Basic.to_string value)]]] [@metaloc loc]

and generate_poly_variant_union config loc name fragments exhaustive_flag =
  let fragment_cases = Ast_helper.(
      fragments
      |> List.map (fun (type_name, inner) -> 
          let name_pattern = Pat.constant (Pconst_string (type_name, None)) in
          let variant = Ast_helper.(Exp.variant type_name (Some (generate_decoder config inner))) in
          Exp.case name_pattern variant)) in
  let fallback_case, fallback_case_ty = Ast_helper.(match exhaustive_flag with
      | Result_structure.Exhaustive ->
        (Exp.case
           (Pat.var { loc = Location.none; txt = "typename" })
           (make_error_raiser loc [%expr
              "Union " ^ [%e const_str_expr name] ^
              " returned unknown type " ^ typename]),
         [ ])
      | Nonexhaustive -> 
        (Exp.case (Pat.any ()) [%expr `Nonexhaustive]), [Rtag ({ txt = "Nonexhaustive"; loc }, [], true, [])]) in
  let fragment_case_tys = List.map
      (fun (name, _) -> Rtag ({ txt = name; loc }, [], false, [{ ptyp_desc = Ptyp_any; ptyp_attributes = []; ptyp_loc = Location.none }])) 
      fragments in
  let union_ty = Ast_helper.(Typ.variant (List.concat [ fallback_case_ty; fragment_case_tys ]) Closed None) in
  let typename_matcher = Ast_helper.(Exp.match_
                                       [%expr typename]
                                       (List.concat [ fragment_cases; [ fallback_case ]])) in
  [%expr
    match value with
    | `Assoc typename_obj -> begin match List.assoc_opt "__typename" typename_obj with
        | None -> [%e make_error_raiser loc [%expr
            "Union " ^ [%e const_str_expr name] ^
            " is missing the __typename field"]]
        | Some typename -> begin match typename with
            | `String typename -> ([%e typename_matcher]: [%t union_ty])
            | None -> [%e make_error_raiser loc [%expr
                "Union " ^ [%e const_str_expr name] ^
                " has a __typename field that is not a string"]]
          end
      end
    | None -> [%e make_error_raiser loc [%expr "Expected union " ^ [%e const_str_expr name] ^ " to be an object, got " ^ (Yojson.Basic.to_string value)]]] [@metaloc loc]
