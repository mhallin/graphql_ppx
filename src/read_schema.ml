exception Schema_file_not_found

let typename_field = {
  Schema.fm_name = "__typename";
  fm_description = None;
  fm_arguments = [];
  fm_field_type = Schema.NonNull (Schema.Named "String");
  fm_deprecation_reason = None
}

let map_items f t =
  let new_t = Hashtbl.create (Hashtbl.length t) in
  let mapper k v =
    let (k', v') = f k v in
    Hashtbl.add new_t k' v'
  in
  let () = Hashtbl.iter mapper t in
  new_t

let map_values f = map_items (fun k v -> (k, f k v))

let some_or o v = match o with
  | Some v -> v
  | None -> v

let add_typename_if_missing fields =
  let open Schema in
  if List.exists (fun ({ fm_name }) -> fm_name = "__typename") fields
    then fields
    else typename_field :: fields

exception Unknown_type_kind of string

let rec type_ref_name tr =
  let open Schema in
  match tr with
  | Named n -> n
  | NonNull i -> type_ref_name i
  | List i -> type_ref_name i

let make_enum_value_meta v =
  let open Yojson.Basic.Util in
  let open Schema in
  {
    evm_name = v |> member "name" |> to_string;
    evm_description = v |> member "description" |> to_string_option;
    evm_deprecation_reason = if (v |> member "isDeprecated" |> to_bool)
      then Some (some_or (v |> member "deprecationReason" |> to_string_option) "(unknown)")
      else None
  }

let rec make_type_ref v =
  let open Yojson.Basic.Util in
  let open Schema in
  match v |> member "kind" |> to_string with
  | "LIST" -> List (v |> member "ofType" |> make_type_ref)
  | "NON_NULL" -> NonNull (v |> member "ofType" |> make_type_ref)
  | _ -> Named (v |> member "name" |> to_string)

let make_argument_meta v =
  let open Yojson.Basic.Util in
  let open Schema in
  {
    am_name = v |> member "name" |> to_string;
    am_description = v |> member "description" |> to_string_option;
    am_arg_type = v |> member "type" |> make_type_ref;
    am_default_value = v |> member "defaultValue" |> to_string_option;
  }

let make_field_meta v =
  let open Yojson.Basic.Util in
  let open Schema in
  {
    fm_name = v |> member "name" |> to_string;
    fm_description = v |> member "description" |> to_string_option;
    fm_arguments = v |> member "args" |> to_list |> List.map make_argument_meta;
    fm_field_type = v |> member "type" |> make_type_ref;
    fm_deprecation_reason = if v |> member "isDeprecated" |> to_bool
      then Some (some_or (v |> member "deprecationReason" |> to_string_option) "(unknown)")
      else None
  }

let make_scalar_meta v =
  let open Yojson.Basic.Util in
  let open Schema in
  {
    sm_name = v |> member "name" |> to_string;
    sm_description = v |> member "description" |> to_string_option;
  }

let make_object_meta v =
  let open Yojson.Basic.Util in
  let open Schema in
  {
    om_name = v |> member "name" |> to_string;
    om_description = v |> member "description" |> to_string_option;
    om_fields = v |> member "fields" |> to_list |> List.map make_field_meta |> add_typename_if_missing;
    om_interfaces = v |> member "interfaces" |> to_list 
                    |> List.map (fun i -> i |> make_type_ref |> type_ref_name)
  }

let make_enum_meta v =
  let open Yojson.Basic.Util in
  let open Schema in
  {
    em_name = v |> member "name" |> to_string;
    em_description = v |> member "description" |> to_string_option;
    em_values = v |> member "enumValues" |> to_list |> List.map make_enum_value_meta;
  }

let make_interface_meta v =
  let open Yojson.Basic.Util in
  let open Schema in
  {
    im_name = v |> member "name" |> to_string;
    im_description = v |> member "description" |> to_string_option;
    im_fields = v |> member "fields" |> to_list |> List.map make_field_meta |> add_typename_if_missing;
  }

let make_union_meta v =
  let open Yojson.Basic.Util in
  let open Schema in
  {
    um_name = v |> member "name" |> to_string;
    um_description = v |> member "description" |> to_string_option;
    um_of_types = v |> member "possibleTypes" |> to_list 
                  |> List.map (fun i -> i |> make_type_ref |> type_ref_name)
  }

let make_input_object_meta v =
  let open Yojson.Basic.Util in
  let open Schema in
  {
    iom_name = v |> member "name" |> to_string;
    iom_description = v |> member "description" |> to_string_option;
    iom_input_fields = v |> member "inputFields" |> to_list |> List.map make_argument_meta;
  }

let make_type_meta _ v =
  let open Yojson.Basic.Util in
  match (v |> member "kind" |> to_string) with
  | "SCALAR" -> Schema.Scalar (make_scalar_meta v)
  | "OBJECT" -> Schema.Object (make_object_meta v)
  | "ENUM" -> Schema.Enum (make_enum_meta v)
  | "INTERFACE" -> Schema.Interface (make_interface_meta v)
  | "UNION" -> Schema.Union (make_union_meta v)
  | "INPUT_OBJECT" -> Schema.InputObject (make_input_object_meta v)
  | e -> raise @@ Unknown_type_kind e

let make_type_map type_array =
  let open Yojson.Basic.Util in
  let rec type_map_loop i type_map =
    if i = Array.length type_array then
      type_map
    else begin
      let () = Hashtbl.add type_map (type_array.(i) |> member "name" |> to_string) type_array.(i) in
      type_map_loop (i + 1) type_map
    end
  in
  let type_map = type_map_loop 0 (Hashtbl.create (Array.length type_array)) in
  map_values make_type_meta type_map

let make_directive_location directive_location =
  let open Schema in
  let open Yojson.Basic.Util in
  match directive_location |> to_string with
  | "QUERY" -> Dl_query
  | "MUTATION" -> Dl_mutation
  | "SUBSCRIPTION" -> Dl_subscription
  | "FIELD" -> Dl_field
  | "FRAGMENT_DEFINITION" -> Dl_fragment_definition
  | "FRAGMENT_SPREAD" -> Dl_fragment_spread
  | "INLINE_FRAGMENT" -> Dl_inline_fragment
  | _ -> Dl_unknown

let make_directive_meta _ directive =
  let open Schema in
  let open Yojson.Basic.Util in
  {
    dm_name = directive |> member "name" |> to_string;
    dm_locations = directive |> member "locations" |> to_list |> List.map make_directive_location;
    dm_arguments = directive |> member "args" |> to_list |> List.map make_argument_meta;
  }

let make_directive_map directive_array =
  let open Yojson.Basic.Util in
  let directive_json_map = Hashtbl.create (Array.length directive_array) in
  let () = Array.iter (fun directive ->
    Hashtbl.add directive_json_map (directive |> member "name" |> to_string) directive) directive_array in
  map_values make_directive_meta directive_json_map

let make_schema_meta v =
  let open Yojson.Basic.Util in
  let open Schema in
  {
    sm_query_type = v |> member "queryType" |> member "name" |> to_string;
    sm_mutation_type = v |> member "mutationType"
                       |> to_option (fun m -> m |> member "name" |> to_string);
    sm_subscription_type = v |> member "subscriptionType"
                           |> to_option (fun s -> s |> member "name" |> to_string);
  }

let rec find_file dir name =
  let here_file = Filename.concat dir name in
  print_endline ("[here_file] " ^ here_file);

  if Sys.file_exists here_file then
    let () = print_endline ("[found] "^here_file) in
    Some here_file
  else if Filename.dirname dir = dir then
    None
  else 
    find_file (Filename.dirname dir) name

let create_marshaled_schema filepath data =
  print_endline ("[write marshaled] "^filepath);
  let outc = open_out filepath in
  Marshal.to_channel outc data [];
  close_out outc

let read_marshaled_schema filepath =
  print_endline ("[read marshaled] "^filepath);
  let file = open_in filepath in
  let data = Marshal.from_channel file in
  close_in file;
  data

let parse_schema filepath =
  print_endline ("[parse raw] "^filepath);
  let result = Yojson.Basic.from_file filepath in
  let open Yojson.Basic.Util in
  let open Schema in
  let schema = result |> member "data" |> member "__schema" in
  {
    meta = make_schema_meta schema;
    type_map = schema |> member "types" |> to_list |> Array.of_list |> make_type_map;
    directive_map = schema |> member "directives" |> to_list |> Array.of_list |> make_directive_map;
  }

let get_schema dir name = lazy (
  match find_file dir (name^".marshaled") with 
    | Some filepath -> read_marshaled_schema filepath
    | None -> match find_file dir name with
        | Some filepath -> 
          let raw_schema = parse_schema filepath in
          create_marshaled_schema (filepath^".marshaled") raw_schema;
          print_endline ("[parse completed] ");
          raw_schema
        | None -> raise Schema_file_not_found
      
)


