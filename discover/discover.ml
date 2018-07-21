module C = Configurator.V1

let () =
  C.main ~name:"graphql_ppx" (fun c ->
    let system = C.ocaml_config_var_exn c "system" in
    let flags = if system = "linux" then ["-ccopt"; "-static"] else [] in
    C.Flags.write_sexp "dune.flags" flags)
