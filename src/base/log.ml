let log msg =
  if Ppx_config.verbose_logging () then print_endline msg

let must_log = print_endline
