open Graphql_ast
open Source_pos
open Schema

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

let is_nullable = function
  | Ntr_named _ | Ntr_list _ -> false
  | Ntr_nullable _ -> true