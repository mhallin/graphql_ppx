exception Option_unwrap_error

let map f = function
  | None -> None
  | Some v -> Some (f v)

let flat_map f = function
  | None -> None
  | Some v -> f v

let unsafe_unwrap = function
  | None -> raise Option_unwrap_error
  | Some v -> v
