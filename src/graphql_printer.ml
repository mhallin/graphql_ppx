open Graphql_ast
open Source_pos

open Schema

(*
  We rely on graphql_ppx.ml having created the result decoder before running
  the functions in this file. The result decoder produces proper and located
  error messages for missing or invalid types, which means that we can - for
  simplicity's sake - skip any kind of error handling here.

  The tricky part is of course to make sure that they rely on the same kind
  of validation logic :/
*)

let rec type_ref_name = function
  | Named n -> n
  | NonNull t -> type_ref_name t
  | List t -> type_ref_name t

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
  match d.item.d_name.item with
  | "bsVariant" | "bsRecord" | "bsDecoder" -> true
  | _ -> false

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

let rec print_selection_set schema ty ss = match ss with
  | [] -> ""
  | l ->
    let add_typename = match ty with | Interface _ | Union _ -> true | _ -> false in
    "{\n" ^ (if add_typename then "__typename\n" else "") ^ (List.map (print_selection schema ty) l |> String.concat "\n") ^ "\n}"

and print_selection schema ty s = match s with 
  | Field { item } -> print_field schema ty item
  | FragmentSpread { item } -> print_fragment_spread item
  | InlineFragment { item } -> print_inline_fragment schema ty item

and print_field schema ty f =
  let ty_fields = Option.unsafe_unwrap @@ match ty with
    | Interface {im_fields} -> Some im_fields
    | Object {om_fields} -> Some om_fields
    | _ -> None
  in
  let field_ty = (List.find (fun fm -> fm.fm_name = f.fd_name.item) ty_fields).fm_field_type
                 |> type_ref_name |> lookup_type schema |> Option.unsafe_unwrap in
  (match f.fd_alias with | Some {item} -> item ^ ": " | None -> "") ^
  f.fd_name.item ^
  (match f.fd_arguments with | Some {item} -> print_arguments item | None -> "") ^
  (print_directives f.fd_directives) ^
  (match f.fd_selection_set with | Some {item} -> print_selection_set schema field_ty item | None -> "")


and print_inline_fragment schema ty f =
  let inner_ty = match f.if_type_condition with | Some {item} -> lookup_type schema item |> Option.unsafe_unwrap | None -> ty in
  "..." ^ 
  (match f.if_type_condition with | Some {item} -> "on " ^ item ^ " " | None -> " ") ^
  (print_directives f.if_directives) ^
  (print_selection_set schema inner_ty f.if_selection_set.item)

let print_variable_definition (name, def) = Printf.sprintf "$%s: %s%s"
    name.item
    (print_type def.vd_type.item)
    (match def.vd_default_value with | Some { item } -> " = " ^ (print_input_value item) | None -> "")

let print_variable_definitions defs =
  "(" ^ (List.map print_variable_definition defs |> String.concat ", ") ^ ")"

let print_operation schema op =
  let ty_name = match op.o_type with
    | Query -> schema.meta.sm_query_type
    | Mutation -> Option.unsafe_unwrap schema.meta.sm_mutation_type
    | Subscription -> Option.unsafe_unwrap schema.meta.sm_subscription_type in
  (match op.o_type with | Query -> "query " | Mutation -> "mutation " | Subscription -> "subscription ") ^
  (match op.o_name with | Some { item } -> item | None -> "") ^
  (match op.o_variable_definitions with | Some { item } -> print_variable_definitions item | None -> "") ^
  (print_directives op.o_directives) ^
  (print_selection_set schema (lookup_type schema ty_name |> Option.unsafe_unwrap) op.o_selection_set.item)

let print_fragment schema f =
  "fragment " ^ f.fg_name.item ^ " on " ^ f.fg_type_condition.item ^ " " ^
  (print_directives f.fg_directives) ^
  (print_selection_set schema (lookup_type schema f.fg_type_condition.item |> Option.unsafe_unwrap) f.fg_selection_set.item)

let print_definition schema def = match def with
  | Operation { item = operation } -> print_operation schema operation
  | Fragment { item = fragment } -> print_fragment schema fragment

let print_document schema defs = List.map (print_definition schema) defs |> String.concat "\n\n"
