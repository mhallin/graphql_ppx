open Ast
open Source_pos
open Schema

open Ast_402
open Parsetree
open Asttypes

open Type_utils
open Generator_utils

exception Unimplemented of string

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




let apply loc func_name expression =
  Pexp_apply (
    make_expression loc (make_expression_from_string loc func_name),
    [("", expression)]
  )

let apply_multi loc func_name expressions =
  Pexp_apply (
    make_expression loc (Pexp_ident { txt = Longident.parse func_name; loc}),
    List.map (fun expression -> ("", expression)) expressions
  )

let apply_js loc x y =
  let make_expression = make_expression loc in
  Pexp_send
    (make_expression 
       (Pexp_apply
          (make_expression (
              Pexp_ident
                Longident.{txt = Ldot (Lident "Js_unsafe", "unsafe_downgrade"); loc}),
           [("", x)])),
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
    type t = string spanning * Schema.type_meta
    let compare (_, x) (_, y) = Schema.compare_type_meta x y
  end
  )

let rec extract_variable_types schema acc =
  function
  | [] -> acc
  | (spanning, type_meta)::t -> 
    let ty = lookup_type schema (
        unwrapped_type_name_of_native_type_ref (to_native_type_ref type_meta)
      ) in
    match ty with
    | None ->
      extract_variable_types schema acc t
    | Some x when (TypeSet.exists (fun (_, y) -> Schema.compare_type_meta x y == 0) acc) ->
      extract_variable_types schema acc t
    | Some InputObject { iom_name } when String.compare iom_name "_QueryMeta" == 0 -> 
      extract_variable_types schema acc t
    | Some Object { om_name } when String.compare om_name "_QueryMeta" == 0 -> 
      extract_variable_types schema acc t
    | Some Object x ->
      let acc = TypeSet.add (spanning, (Object x)) acc in
      let child_types = List.map (fun x -> (spanning, x.fm_field_type)) x.om_fields in
      extract_variable_types schema (extract_variable_types schema acc child_types) t
    | Some InputObject x ->
      let acc = TypeSet.add (spanning, (InputObject x)) acc in
      let child_types = List.map (fun x -> (spanning, x.am_arg_type)) x.iom_input_fields in
      extract_variable_types schema (extract_variable_types schema acc child_types) t
    | Some x ->
      let acc = TypeSet.add (spanning, x) acc in
      extract_variable_types schema acc t

let make_value_binding loc pattern expression = {
  pvb_pat = pattern;
  pvb_expr = expression;
  pvb_attributes = [];
  pvb_loc = loc;
}

let make_function loc ~from:name expression =
  let make_pattern = make_pattern loc in
  make_expression loc (Pexp_fun ("", None, make_pattern (Ppat_var {loc; txt = name; }), expression))

let function_name_string x = "json_of_" ^ Schema.extract_name_from_type_meta x

let rec parser_for_type schema loc type_ref = 
  let make_expression = make_expression loc in
  let raise_inconsistent_schema type_name = raise_error_with_loc loc ("Inconsistent schema, type named " ^ type_name ^ " cannot be found") in
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
      | None -> raise_inconsistent_schema (unwrapped_type_name_of_native_type_ref x)
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
    | None -> raise_inconsistent_schema type_name 
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
                    make_expression (Pexp_constant (Const_string (am_name, None)));
                    make_expression @@ Pexp_apply (
                      parser_,
                      [("", make_expression @@ apply_js loc expr am_name)]
                    )
                  ]
                );
                list_of_fields schema loc expr t
              ]
            ))))

let generate_encoder schema map_loc (spanning, x) =
  let loc = map_loc spanning.span in
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
                          ("", {
                              pexp_desc = Pexp_constant (Const_string(evm_name, None));
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
          "",
          make_type (
            Ptyp_arrow (
              "", make_type (Ptyp_var "a"), json_t)
          ),
          make_type (
            Ptyp_arrow (
              "",
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
              apply_multi "Js.Array.map"
                [
                  make_value_expression "encoder";
                  make_value_expression "value"
                ]
            ))))))

let rec determine_is_recursive (_, type_) prev =
  match type_ with
  | Scalar _ -> prev
  | Enum _ -> prev
  | Object _
  | InputObject _
  | Interface _
  | Union _ -> Recursive

let generate_encoders schema loc map_loc = 
  function
  | [Operation { 
      item = { o_variable_definitions = Some { item } }
    }] -> List.map (fun (spanning, {vd_type = variable_type}) -> 
      (spanning, to_schema_type_ref variable_type.item)
    ) item 
          |> extract_variable_types schema TypeSet.empty
          |> (fun types -> (
                TypeSet.fold determine_is_recursive types Nonrecursive, 
                TypeSet.fold (fun element t -> (generate_encoder schema map_loc element)::t) types [])
            )
          |> (fun (rec_flag, encoders) -> (rec_flag, (optional_encoder loc)::(array_encoder loc)::encoders))
  | [Operation { item = { o_variable_definitions = None }}] -> (Nonrecursive, [])
  | _ -> raise @@ Unimplemented "variables on other than singular queries/mutations" 
