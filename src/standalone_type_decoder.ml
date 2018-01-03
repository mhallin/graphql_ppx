module StringSet = Set.Make(String)

let rec complete_selection_set map_loc span schema typ selection_set_set =
  let open Type_utils in
  let open Source_pos in
  let open Schema in
  let native_type_ref = 
    to_native_type_ref typ in
  let name = unwrapped_type_name_of_native_type_ref native_type_ref in
  let schema_typ = Schema.lookup_type schema name in
  match schema_typ with
  | None -> Generator_utils.raise_error map_loc span ("Could not find type " ^ name)
  | Some Interface _ -> 
      Generator_utils.raise_error 
        map_loc 
        span 
        ("Creating standalone parsers for interfaces is currently impossible")
  | Some InputObject _ -> 
      Generator_utils.raise_error 
        map_loc 
        span 
        ("Creating standalone parsers for input objects is currently impossible")
  | Some Union _ -> 
      Generator_utils.raise_error 
        map_loc 
        span 
        ("Creating standalone parsers for unions is currently impossible")
  | Some Object { om_fields } ->
    let open Ast in
    Some {
      span = span;
      item = List.map (fun field -> 
        if StringSet.mem name selection_set_set then 
          None
        else
          Some (Field {
            span = span;
            item = {
              fd_alias = None;
              fd_name = {
                span = span;
                item = field.fm_name
              };
              fd_arguments = None;
              fd_directives = [];
              fd_selection_set = 
                complete_selection_set 
                  map_loc 
                  span 
                  schema 
                  field.fm_field_type 
                  (StringSet.add name selection_set_set);
            }
          })
      ) om_fields 
      |> List.fold_left (fun t -> function
        | Some x -> x::t
        | None -> t
      ) []
    }
  | _ -> None
  

let parser_for_type typ schema map_loc =
  let open Type_utils in
  let open Source_pos in
  let open Schema in
  let schema_typ = (to_schema_type_ref typ.item) in
  Result_decoder.unify_type 
  false
  map_loc
  typ.span
  (to_native_type_ref schema_typ)
  schema
  (complete_selection_set map_loc typ.span schema schema_typ StringSet.empty) 
