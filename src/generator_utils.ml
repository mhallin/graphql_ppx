let raise_error_with_loc loc message =
  raise (Location.Error (
      Location.error ~loc message
    ))

let raise_error map_loc span message =
  raise_error_with_loc (map_loc span) message

let some_or o d = match o with
  | Some v -> v
  | None -> d
