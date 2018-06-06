type error_marker = {
  mutable has_error: bool
}

let raise_error_with_loc loc message =
  raise (Location.Error (
      Location.error ~loc message
    ))

let raise_error map_loc span message =
  raise_error_with_loc (map_loc span) message

let error_expr marker map_loc span message =
  let () = marker.has_error <- true in
  let loc = map_loc span in
  let ext = Ast_mapper.extension_of_error (Location.error ~loc message) in
  [%expr let _value = value in [%e Ast_helper.Exp.extension ~loc ext]]

let some_or o d = match o with
  | Some v -> v
  | None -> d
