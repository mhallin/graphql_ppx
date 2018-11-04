type type_ref =
  | Named of string
  | NonNull of type_ref
  | List of type_ref

type argument_meta = {
  am_name: string;
  am_description: string option;
  am_arg_type: type_ref;
  am_default_value: string option;
}

type field_meta = {
  fm_name: string;
  fm_description: string option;
  fm_arguments: argument_meta list;
  fm_field_type: type_ref;
  fm_deprecation_reason: string option;
}

type scalar_meta = {
  sm_name: string;
  sm_description: string option;
}

type object_meta = {
  om_name: string;
  om_description: string option;
  om_fields: field_meta list;
  om_interfaces: string list;
}

type enum_value_meta = {
  evm_name: string;
  evm_description: string option;
  evm_deprecation_reason: string option;
}

type enum_meta = {
  em_name: string;
  em_description: string option;
  em_values: enum_value_meta list;
}

type interface_meta = {
  im_name: string;
  im_description: string option;
  im_fields: field_meta list;
}

type union_meta = {
  um_name: string;
  um_description: string option;
  um_of_types: string list;
}

type input_object_meta = {
  iom_name: string;
  iom_description: string option;
  iom_input_fields: argument_meta list;
}

type type_meta =
  | Scalar of scalar_meta
  | Object of object_meta
  | Enum of enum_meta
  | Interface of interface_meta
  | Union of union_meta
  | InputObject of input_object_meta

type schema_meta = {
  sm_query_type: string;
  sm_mutation_type: string option;
  sm_subscription_type: string option;
}

type directive_location =
  | Dl_query
  | Dl_mutation
  | Dl_subscription
  | Dl_field
  | Dl_fragment_definition
  | Dl_fragment_spread
  | Dl_inline_fragment
  | Dl_unknown

type directive_meta = {
  dm_name: string;
  dm_locations: directive_location list;
  dm_arguments: argument_meta list;
}

type schema = {
  meta: schema_meta;
  type_map: (string, type_meta) Hashtbl.t;
  directive_map: (string, directive_meta) Hashtbl.t;
}

let query_type s = Hashtbl.find s.type_map s.meta.sm_query_type
let mutation_type s = match s.meta.sm_mutation_type with
  | Some n -> Some (Hashtbl.find s.type_map n)
  | None -> None 
let subscription_type s = match s.meta.sm_subscription_type with
  | Some n -> Some (Hashtbl.find s.type_map n)
  | None -> None

exception Invalid_type of string
exception Inconsistent_schema of string


let lookup_implementations schema im =  
  let all_objects_implementing_interface _ value acc = match value with
    | Object { om_interfaces; _ } as o when List.exists (fun n -> n = im.im_name) om_interfaces -> o :: acc
    | _ -> acc
  in
  Hashtbl.fold all_objects_implementing_interface schema.type_map []

let lookup_field ty name = 
  let find_field fs = 
    match List.find_all (fun f -> f.fm_name = name) fs with
    | [] -> None
    | [x] -> Some x
    | _ -> raise @@ Inconsistent_schema ("Multiple fields named " ^ name)
  in
  match ty with
  | Object { om_fields; _ } -> find_field om_fields
  | Interface { im_fields; _ } -> find_field im_fields
  | Scalar { sm_name; _ } -> raise @@ Invalid_type ("Type " ^ sm_name ^ " doesn't have any fields")
  | Enum { em_name; _ } -> raise @@ Invalid_type ("Type " ^ em_name ^ " doesn't have any fields")
  | Union { um_name; _ } -> raise @@ Invalid_type ("Type " ^ um_name ^ " doesn't have any fields")
  | InputObject { iom_name; _ } -> raise @@ Invalid_type ("Type " ^ iom_name ^ " doesn't have any fields")

let lookup_input_field ty name =
  let find_field fs =
    match List.find_all (fun am -> am.am_name = name) fs with
    | [] -> None
    | [x] -> Some x
    | _ -> raise @@ Inconsistent_schema ("Multiple input fields named " ^ name)
  in
  match ty with
  | Object { om_name = name; _ }
  | Interface { im_name = name; _ }
  | Scalar { sm_name = name; _ }
  | Enum { em_name = name; _ }
  | Union { um_name = name; _ } -> raise @@ Invalid_type ("Type " ^ name ^ " doesn't have any input fields")
  | InputObject { iom_input_fields; _ } -> find_field iom_input_fields

let type_name ty = match ty with
  | Scalar { sm_name; _ } -> sm_name
  | Object { om_name; _ } -> om_name
  | Enum { em_name; _ } -> em_name
  | Interface { im_name; _ } -> im_name
  | Union { um_name; _ } -> um_name
  | InputObject { iom_name; _ } -> iom_name

let lookup_type schema name =
  match (Hashtbl.find_all schema.type_map name) with
  | [] -> None
  | [x] -> Some x
  | _ -> raise @@ Inconsistent_schema ("Multiple types named " ^ name)

let lookup_directive schema name =
  match (Hashtbl.find_all schema.directive_map name) with
  | [] -> None
  | [x] -> Some x
  | _ -> raise @@ Inconsistent_schema ("Multiple directives named " ^ name)

let all_enums schema = Hashtbl.fold (fun _ v acc ->
    match v with
    | Enum e -> e :: acc
    | _ -> acc) schema.type_map []

let extract_name_from_type_meta = function
  | Scalar { sm_name = x; _ }
  | Object { om_name = x; _ }
  | Enum { em_name = x; _ }
  | Interface { im_name = x; _ }
  | Union { um_name = x; _ }
  | InputObject {iom_name = x; _ } -> x

let compare_type_meta x y =
  String.compare (extract_name_from_type_meta x) (extract_name_from_type_meta y)

let rec innermost_name = function
  | Named name -> name
  | NonNull inner | List inner -> innermost_name inner
