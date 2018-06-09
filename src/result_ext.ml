open Result

let flat_map f = function
  | Error e -> Error e
  | Ok v -> f v

let map f = function
  | Error e -> Error e
  | Ok v -> Ok (f v)

let replace v = function
  | Error e -> Error e
  | Ok _ -> Ok v

let make_t2 a b = (a, b)
let make_t3 a b c = (a, b, c)
let make_t4 a b c d = (a, b, c, d)
let make_t5 a b c d e = (a, b, c, d, e)
