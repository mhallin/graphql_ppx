module To_current = Migrate_parsetree.Convert(Migrate_parsetree.OCaml_402)(Migrate_parsetree.OCaml_current)

open Source_pos

let add_pos delimLength base pos =
  let open Lexing in
  let (_, _, col) = Location.get_pos_info base in
  {
    pos_fname = base.pos_fname;
    pos_lnum = base.pos_lnum + pos.line;
    pos_bol = 0;
    pos_cnum = if pos.line == 0 then delimLength + col + pos.col else pos.col;
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

let rec find_schema_file dir =
  let here_file = Filename.concat dir "graphql_schema.json" in
  if Sys.file_exists here_file then
    Some here_file
  else if Filename.dirname dir = dir then
    None
  else 
    find_schema_file (Filename.dirname dir)

exception Schema_file_not_found

let mapper argv =
  let open Ast_402 in
  let open Asttypes in
  let open Parsetree in
  let open Ast_mapper in
  let open Ast_helper in
  let open Parsetree in
  let open Location in
  let open Asttypes in

  let schema = match find_schema_file (Sys.getcwd ()) with
    | Some filename -> Read_schema.read_schema_file filename
    | None -> raise Schema_file_not_found
  in

  let is_ast_output = match List.find ((=) "-ast-out") argv with
    | _ -> true
    | exception Not_found -> false
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
                let error_marker = { Generator_utils.has_error = false } in
                let parse_fn = Result_decoder.unify_document_schema error_marker (add_loc delimLength loc) schema document in
                if error_marker.Generator_utils.has_error then
                  Mod.mk
                    (Pmod_structure [
                        [%stri exception Graphql_error of string];
                        [%stri let parse = fun value -> [%e Output_bucklescript_decoder.generate_decoder parse_fn]];
                      ])
                else
                  let (rec_flag, encoders) = 
                    Variable_encoder.generate_encoders schema loc (add_loc delimLength loc) document in
                  let reprinted_query = Graphql_printer.print_document schema document in
                  let make_fn, make_with_variables_fn = Unifier.make_make_fun (add_loc delimLength loc) schema document in
                  Mod.mk
                    (Pmod_structure [
                        [%stri exception Graphql_error of string];
                        [%stri let ppx_printed_query = [%e if is_ast_output
                                 then Ast_serializer_apollo.serialize_document query document
                                 else Exp.constant (Const_string (reprinted_query, delim))]];
                        [%stri let query = ppx_printed_query];
                        [%stri let parse = fun value -> [%e Output_bucklescript_decoder.generate_decoder parse_fn]];

                        {
                          pstr_desc = (Pstr_value (rec_flag, encoders));
                          pstr_loc = Location.none;
                        };
                        [%stri let make = [%e make_fn]];
                        [%stri let makeWithVariables = [%e make_with_variables_fn]];

                        (* Some functor magic to determine the return type of parse *)
                        [%stri module type mt_ret = sig type t end];
                        [%stri type 'a typed_ret = (module mt_ret with type t = 'a)];
                        [%stri let ret_type (type a) (f: _ -> a) = (let module MT_Ret = struct type t = a end in (module MT_Ret): a typed_ret)];
                        [%stri module MT_Ret = (val ret_type parse)];
                        [%stri type t = MT_Ret.t];
                      ])
          end
        | _ -> raise (Location.Error (
            Location.error ~loc "[%graphql] accepts a string, e.g. [%graphql {| { query |}]"
          ))
      end
    | other -> default_mapper.module_expr mapper other
  end in

  To_current.copy_mapper { default_mapper with module_expr }

let () = Migrate_parsetree.Compiler_libs.Ast_mapper.register "graphql" (fun argv -> mapper argv)
