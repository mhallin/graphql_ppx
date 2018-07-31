type global_options = {
  mutable verbose: bool;
}

let global_options = {
  verbose = false;
}

let verbose b checker =
  let () = global_options.verbose <- b in checker

let log_head = "[dirty_checker]"
let log msg = if global_options.verbose then print_endline (log_head^msg)
let must_log msg = print_endline (log_head^msg)

exception File_not_found

let hash file =
  let () = log ("[calc hash of] "^file) in
  Digest.file file

let read_hash src =
  let () = log ("[read hash from] "^src) in
  let hash_file = open_in src in
  let hash = Digest.input hash_file in
  close_in hash_file;
  hash

let write_hash hash dest =
  let () = log ("[write hash to] "^dest) in
  let dest = open_out dest in
  Digest.output dest hash;
  close_out dest

let find_file file =
  if Sys.file_exists file then
    let () = log ("[found] "^file) in
    Some file
  else
    let () = log ("[not found] "^file) in
    None

let get_rel_hash_path file = 
  Filename.concat 
    (Filename.dirname file)
    ("."^(Filename.basename file)^".hash")

type checker = {
  src: string;
  hash_path: string;
  dirty_callback: string -> unit;
}

let makeDefaultChecker file = {
  src = file;
  hash_path = get_rel_hash_path file;
  dirty_callback = fun _ -> ();
}

let make file = makeDefaultChecker file

let set_hash_path path checker = { checker with hash_path = path}

let on_dirty cb checker = {checker with dirty_callback = cb }

exception Dirty_update_failure of string

let dirty_update hash checker =
  log "[resource dirty!]";
  match checker.dirty_callback checker.src with
  | () ->
    write_hash hash checker.hash_path;
    must_log "[update ok]"
  | exception Dirty_update_failure msg ->
    must_log ("[update error] "^msg)

let check checker =
  match find_file checker.src with
  | None -> raise File_not_found
  | Some file ->
      let new_hash = hash file in
      match find_file (checker.hash_path) with
      | None -> dirty_update new_hash checker (* dirty *)
      | Some hash_file ->
          let old_hash = read_hash hash_file in
          match Digest.compare old_hash new_hash with
          | 0 -> log "[resource unchanged]"
          | _ -> dirty_update new_hash checker (* dirty *)
