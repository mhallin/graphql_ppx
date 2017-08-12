open Ast
open Source_pos

let rec print_input_value iv = match iv with
  | Iv_null -> "null"
  | Iv_int i -> string_of_int i
  | Iv_float f -> string_of_float f
  | Iv_string s -> "\"" ^ (String.escaped s) ^ "\""
  | Iv_boolean b -> string_of_bool b
  | Iv_enum s -> s
  | Iv_variable v -> "$" ^ v
  | Iv_list l -> 
    "[" ^ (List.map (fun {item} -> print_input_value item) l |> String.concat ", ") ^ "]"
  | Iv_object o ->
    "{" ^ (List.map (fun ({item=key}, {item=value}) -> key ^ ": " ^ print_input_value value) o |> String.concat ", ") ^ "}"

let print_argument ({item=key}, {item=value}) =
  key ^ ": " ^ print_input_value value

let print_arguments args = match args with
  | [] -> ""
  | args -> "(" ^ (List.map print_argument args |> String.concat ", ") ^ ")"

let print_directive d =
  "@" ^ d.d_name.item ^
  (match d.d_arguments with | Some {item} -> print_arguments item | None -> "")

let is_internal_directive d =
  d.item.d_name.item = "bsVariant"

let print_directives ds =
  " " ^ (ds |> List.filter (fun d -> not @@ is_internal_directive d)
         |> List.map (fun d -> print_directive d.item) 
         |> String.concat " ") ^ " "

let print_fragment_spread s =
  "..." ^ s.fs_name.item ^ " " ^ (print_directives s.fs_directives)

let rec print_type ty = match ty with
  | Tr_named n -> n.item
  | Tr_list l -> "[" ^ (print_type l.item) ^ "]"
  | Tr_non_null_list l -> "[" ^ (print_type l.item) ^ "]!"
  | Tr_non_null_named n -> n.item ^ "!"

let rec print_selection_set ss = match ss with
  | [] -> ""
  | l -> "{\n" ^ (List.map print_selection l |> String.concat "\n") ^ "\n}"

and print_selection s = match s with 
  | Field { item } -> print_field item
  | FragmentSpread { item } -> print_fragment_spread item
  | InlineFragment { item } -> print_inline_fragment item

and print_field f =
  (match f.fd_alias with | Some {item} -> item ^ ": " | None -> "") ^
  f.fd_name.item ^
  (match f.fd_arguments with | Some {item} -> print_arguments item | None -> "") ^
  (print_directives f.fd_directives) ^
  (match f.fd_selection_set with | Some {item} -> print_selection_set item | None -> "")


and print_inline_fragment f =
  "..." ^ 
  (match f.if_type_condition with | Some {item} -> "on " ^ item ^ " " | None -> " ") ^
  (print_directives f.if_directives) ^
  (print_selection_set f.if_selection_set.item)

let print_variable_definition (name, def) = Printf.sprintf "%s: %s%s"
    name.item
    (print_type def.vd_type.item)
    (match def.vd_default_value with | Some { item } -> " = " ^ (print_input_value item) | None -> "")

let print_variable_definitions defs =
  "(" ^ (List.map print_variable_definition defs |> String.concat ", ") ^ ")"

let print_operation op =
  (match op.o_type with | Query -> "query " | Mutation -> "mutation ") ^
  (match op.o_name with | Some { item } -> item | None -> "") ^
  (match op.o_variable_definitions with | Some { item } -> print_variable_definitions item | None -> "") ^
  (print_directives op.o_directives) ^
  (print_selection_set op.o_selection_set.item)

let print_fragment f =
  "fragment " ^ f.fg_name.item ^ " on " ^ f.fg_type_condition.item ^ " " ^
  (print_directives f.fg_directives) ^
  (print_selection_set f.fg_selection_set.item)

let print_definition def = match def with
  | Operation { item = operation } -> print_operation operation
  | Fragment { item = fragment } -> print_fragment fragment

let print_document defs = List.map print_definition defs |> String.concat "\n\n"
