open Graphql_ast
open Source_pos
open Schema
open Generator_utils

open Ast_402
open Parsetree
open Asttypes

open Type_utils
open Generator_utils

let mangle_enum_name = Generator_utils.uncapitalize_ascii

let ident_from_string loc func_name =
  Ast_helper.(Exp.ident ~loc { txt = Longident.parse func_name; loc })

module StringSet = Set.Make(String)

let sort_variable_types schema variables =
  let recursive_flag = ref false in
  let ordered_nodes = Queue.create () in
  let has_added_to_queue name = Queue.fold (fun acc (_, v) -> acc || name = v) false ordered_nodes in
  let rec loop visit_stack = function
    | [] -> ()
    | (span, type_ref) :: tail ->
      let type_name = innermost_name type_ref in
      let () = match lookup_type schema type_name with
        | None -> ()
        | Some _ when StringSet.mem type_name visit_stack -> recursive_flag := true
        | Some _ when has_added_to_queue type_name -> ()
        | Some (Enum _) -> Queue.push (span, type_name) ordered_nodes
        | Some (InputObject io) ->
          let () = loop
              (StringSet.add type_name visit_stack)
              (List.map (fun { am_arg_type } -> (span, am_arg_type)) io.iom_input_fields) in
          Queue.push (span, type_name) ordered_nodes
        | Some _ -> ()
      in loop visit_stack tail
  in
  let () = loop StringSet.empty variables in
  let () = Queue.iter (fun (_, name) -> Printf.printf "ordered type: %s\n" name) ordered_nodes in
  let ordered_nodes = Array.init
      (Queue.length ordered_nodes)
      (fun _ -> 
         let span, name = Queue.take ordered_nodes in
         (span, name |> lookup_type schema |> Option.unsafe_unwrap)) in
  (! recursive_flag, ordered_nodes)

let function_name_string x = "json_of_" ^ Schema.extract_name_from_type_meta x

let rec parser_for_type schema loc type_ref = 
  let raise_inconsistent_schema type_name = raise_error_with_loc loc ("Inconsistent schema, type named " ^ type_name ^ " cannot be found") in
  match type_ref with
  | Ntr_list x ->
    let child_parser = parser_for_type schema loc x in
    [%expr fun v -> Js.Json.array (Js.Array.map [%e child_parser] v)] [@metaloc loc]
  | Ntr_nullable x ->
    let child_parser = parser_for_type schema loc x in
    [%expr fun v -> match v with None -> Js.Json.null | Some v -> [%e child_parser] v] [@metaloc loc]
  | Ntr_named type_name ->
    match lookup_type schema type_name  with
    | None -> raise_inconsistent_schema type_name 
    | Some (Scalar { sm_name = "String" })
    | Some (Scalar { sm_name = "ID" }) -> [%expr Js.Json.string]
    | Some (Scalar { sm_name = "Int" }) -> [%expr fun v -> Js.Json.number (float_of_int v)]
    | Some (Scalar { sm_name = "Float" }) -> [%expr Js.Json.number]
    | Some (Scalar { sm_name = "Boolean" }) -> [%expr Js.Json.boolean]
    | Some ty ->
      function_name_string ty |> ident_from_string loc

let json_of_fields schema loc expr fields =
  let field_array_exprs = fields |> List.map
                            (fun {am_name; am_arg_type} ->
                               let type_ref = to_native_type_ref am_arg_type in
                               let parser = parser_for_type schema loc type_ref in
                               [%expr (
                                 [%e Ast_helper.Exp.constant (Const_string (am_name, None)) ],
                                 [%e parser] ([%e expr] ## [%e ident_from_string loc am_name])
                               )] [@metaloc loc]) in
  let field_array = Ast_helper.Exp.array field_array_exprs in
  [%expr Js.Json.object_ (Js.Dict.fromArray [%e field_array])] [@metaloc loc]

let generate_encoder config (spanning, x) =
  let loc = config.map_loc spanning.span in
  let body = match x with
    | Scalar _ -> raise_error_with_loc loc "Can not build variable encoder for scalar type"
    | Object _ -> raise @@ Invalid_argument "Unsupported variable type: Object"
    | Interface _ -> raise @@ Invalid_argument "Unsupported variable type: Interface"
    | Union _ -> raise @@ Invalid_argument "Unsupported variable type: Union"
    | Enum { em_values } ->
      let match_arms = em_values |> List.map
                         (fun { evm_name } ->
                            let pattern = Ast_helper.Pat.variant evm_name None in
                            let expr = Ast_helper.Exp.constant (Const_string (evm_name, None)) in
                            Ast_helper.Exp.case pattern [%expr Js.Json.string [%e expr]]) in
      Ast_helper.Exp.match_ [%expr value] match_arms
    | InputObject { iom_input_fields } -> 
      json_of_fields config.schema loc [%expr value] iom_input_fields
  in
  Ast_helper.Vb.mk ~loc (Ast_helper.Pat.var { txt = function_name_string x; loc }) [%expr fun value -> [%e body]]

let rec is_type_recursive schema ts ty =
  match ty with
  | Scalar _
  | Enum _
  | Object _
  | Interface _
  | Union _ -> false
  | InputObject { iom_name; iom_input_fields } ->
    if StringSet.mem iom_name ts then true
    else iom_input_fields |> List.exists (fun { am_arg_type } ->
        let ty = to_native_type_ref am_arg_type
                 |> unwrapped_type_name_of_native_type_ref
                 |> Schema.lookup_type schema
                 |> Option.unsafe_unwrap in
        is_type_recursive schema (StringSet.add iom_name ts) ty)

let generate_encoders config loc = function
  | Some { item } ->
    item
    |> List.map (fun (span, {vd_type = variable_type}) -> span, to_schema_type_ref variable_type.item)
    |> sort_variable_types config.schema
    |> (fun (is_recursive, types) -> (if is_recursive then Recursive else Nonrecursive), Array.map (generate_encoder config) types)
  | None -> (Nonrecursive, [||])
