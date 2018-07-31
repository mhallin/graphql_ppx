module To_current = Migrate_parsetree.Convert(Migrate_parsetree.OCaml_402)(Migrate_parsetree.OCaml_current)

open Source_pos

let add_pos delimLength base pos =
  let open Lexing in
  let (_, _, col) = Location.get_pos_info base in
  {
    pos_fname = base.pos_fname;
    pos_lnum = base.pos_lnum + pos.line;
    pos_bol = 0;
    pos_cnum = if pos.line = 0 then delimLength + col + pos.col else pos.col;
  }

let add_loc delimLength base span =
  let open Location in
  {
    loc_start = add_pos delimLength base.loc_start (fst span);
    loc_end = add_pos delimLength base.loc_start (snd span);
    loc_ghost = false;
  }

let fmt_lex_err err =
  let open Graphql_lexer in
  match err with
  | Unknown_character ch -> Printf.sprintf "Unknown character %c" ch
  | Unexpected_character ch -> Printf.sprintf "Unexpected character %c" ch
  | Unterminated_string -> Printf.sprintf "Unterminated string literal"
  | Unknown_character_in_string ch -> Printf.sprintf "Unknown character in string literal: %c" ch
  | Unknown_escape_sequence s -> Printf.sprintf "Unknown escape sequence in string literal: %s" s
  | Unexpected_end_of_file -> Printf.sprintf "Unexpected end of query"
  | Invalid_number -> Printf.sprintf "Invalid number"

let fmt_parse_err err =
  let open Graphql_parser in
  match err with
  | Unexpected_token t -> Printf.sprintf "Unexpected token %s" (Graphql_lexer.string_of_token t)
  | Unexpected_end_of_file -> "Unexpected end of query"
  | Lexer_error err -> fmt_lex_err err

let make_error_expr loc message =
  let ext = Ast_mapper.extension_of_error (Location.error ~loc message) in
  Ast_helper.Exp.extension ~loc ext

let is_prefixed prefix str =
  let i = 0 in
  let len = String.length prefix in
  let j = ref 0 in
  while !j < len && String.unsafe_get prefix !j =
                    String.unsafe_get str (i + !j) do
    incr j
  done;
  (!j = len)

let drop_prefix prefix str =
  let len = String.length prefix in
  let rest = (String.length str) - len in
  String.sub str len rest

let mapper argv =
  let open Ast_402 in
  let open Asttypes in
  let open Parsetree in
  let open Ast_mapper in
  let open Ast_helper in
  let open Parsetree in
  let open Location in
  let open Asttypes in

  let is_ast_output = match List.find ((=) "-ast-out") argv with
    | _ -> true
    | exception Not_found -> false
  in

  let () = 
  Log.is_verbose := match List.find ((=) "-verbose") argv with
    | _ -> true
    | exception Not_found -> false
  in

  let here_dir = Sys.getcwd() in

  let here_scheme_path = match List.find (is_prefixed "-schema=") argv with
    | arg -> drop_prefix "-schema=" arg
    | exception Not_found -> "graphql_schema.json" (* the default path so it won't break backward compatibility *)
  in

  let module_expr mapper mexpr = begin
    match mexpr with
    | {pmod_desc = Pmod_extension ({txt = "graphql"; loc}, pstr)} -> begin
        match pstr with
        | PStr [{ pstr_desc = Pstr_eval ({
            pexp_loc = loc; 
            pexp_desc = Pexp_constant (Const_string (query, delim))}, _)}] -> begin
            let lexer = Graphql_lexer.make query in
            let delimLength = match delim with | Some s -> 2 + String.length s | None -> 1 in
            match Graphql_lexer.consume lexer with
            | Result.Error e -> raise (Location.Error (
                Location.error ~loc:(add_loc delimLength loc e.span) (fmt_lex_err e.item)
              ))
            | Result.Ok tokens -> 
              let parser = Graphql_parser.make tokens in
              match Graphql_parser_document.parse_document parser with
              | Result.Error e -> raise (Location.Error (
                  Location.error ~loc:(add_loc delimLength loc e.span) (fmt_parse_err e.item)
                ))
              | Result.Ok document ->
                let config = {
                  Generator_utils.map_loc = add_loc delimLength loc;
                  delimiter = delim;
                  output_mode = if is_ast_output then Generator_utils.Apollo_AST else Generator_utils.String;
                  full_document = document;
                  (*  the only call site of schema, make it lazy! *)
                  schema = Lazy.force (Read_schema.get_schema here_dir here_scheme_path);
                } in
                match Validations.run_validators config document with
                | Some errs ->
                  Mod.mk
                    (Pmod_structure (List.map (fun (loc, msg) ->
                         [%stri let _ = [%e make_error_expr loc msg]]) errs))
                | None ->
                  let parts = Result_decoder.unify_document_schema config document in
                  Output_bucklescript_module.generate_modules config parts
          end
        | _ -> raise (Location.Error (
            Location.error ~loc "[%graphql] accepts a string, e.g. [%graphql {| { query |}]"
          ))
      end
    | other -> default_mapper.module_expr mapper other
  end in

  To_current.copy_mapper { default_mapper with module_expr }

let () = Migrate_parsetree.Compiler_libs.Ast_mapper.register "graphql" (fun argv -> mapper argv)

