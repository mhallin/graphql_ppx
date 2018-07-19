module C = Configurator.V1


let process_output_of command =
  let chan = Unix.open_process_in command in
  let res = ref ([] : string list) in
  let rec process_another_line () =
    let e = input_line chan in
    res := e::!res;
    process_another_line() in
  try process_another_line ()
  with End_of_file ->
    let code = Unix.close_process_in chan in (List.rev !res, code)

let output_text command =
  let (l, _) = process_output_of command in String.trim (String.concat "\n" l)


let output = output_text "uname -s"

let _ =
  if output = "Linux" then
    Configurator.V1.Flags.write_sexp "dune.flags" ["-ccopt"; "-static"]
  else
    Configurator.V1.Flags.write_sexp "dune.flags" []
