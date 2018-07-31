let is_verbose = ref false

let log msg = if !is_verbose then print_endline msg

let must_log msg = print_endline msg


