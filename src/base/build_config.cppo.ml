#if OCAML_VERSION >= (4, 3, 0)

let capitalize_ascii = String.capitalize_ascii
let uncapitalize_ascii = String.uncapitalize_ascii
let is_bucklescript = false

#else

let capitalize_ascii = String.capitalize
let uncapitalize_ascii = String.uncapitalize
let is_bucklescript = true

#endif
