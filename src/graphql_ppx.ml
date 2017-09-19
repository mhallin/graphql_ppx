open Migrate_parsetree

module To_current = Convert(OCaml_404)(OCaml_current)

open Ast_404

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
  let open Gql_lexer in
  match err with
  | Unknown_character ch -> Printf.sprintf "Unknown character %c" ch
  | Unexpected_character ch -> Printf.sprintf "Unexpected character %c" ch
  | Unterminated_string -> Printf.sprintf "Unterminated string literal"
  | Unknown_character_in_string ch -> Printf.sprintf "Unknown character in string literal: %c" ch
  | Unknown_escape_sequence s -> Printf.sprintf "Unknown escape sequence in string literal: %s" s
  | Unexpected_end_of_file -> Printf.sprintf "Unexpected end of query"
  | Invalid_number -> Printf.sprintf "Invalid number"

let fmt_parse_err err =
  let open Gql_parser in
  match err with
  | Unexpected_token t -> Printf.sprintf "Unexpected token %s" (Gql_lexer.string_of_token t)
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

let mapper () =
  let open Ast_mapper in
  let open Ast_helper in
  let open Parsetree in
  let open Location in
  let open Asttypes in

  let schema = match find_schema_file (Sys.getcwd ()) with
    | Some filename -> Read_schema.read_schema_file filename
    | None -> raise Schema_file_not_found
  in

  let module_expr mapper mexpr = begin
    match mexpr with
    | {pmod_desc = Pmod_extension ({txt = "graphql"; loc}, pstr)} -> begin
        match pstr with
        | PStr [{ pstr_desc = Pstr_eval ({
            pexp_loc = loc; 
            pexp_desc = Pexp_constant (Pconst_string (query, delim))}, _)}] -> begin
            let lexer = Gql_lexer.make query in
            let delimLength = match delim with | Some s -> 2 + String.length s | None -> 1 in
            match Gql_lexer.consume lexer with
            | Result.Error e -> raise (Location.Error (
                Location.error ~loc:(add_loc delimLength loc e.span) (fmt_lex_err e.item)
              ))
            | Result.Ok tokens -> 
              let parser = Gql_parser.make tokens in
              match Document.parse_document parser with
              | Result.Error e -> raise (Location.Error (
                  Location.error ~loc:(add_loc delimLength loc e.span) (fmt_parse_err e.item)
                ))
              | Result.Ok document ->
                let reprinted_query = Gql_printer.print_document document in
                Mod.mk ~loc (Pmod_structure [
                    {pstr_desc = Pstr_exception {
                         pext_name = { txt = "Graphql_error"; loc = loc};
                         pext_kind = Pext_decl (Pcstr_tuple [], None);
                         pext_loc = loc;
                         pext_attributes = [];
                       }; pstr_loc = loc;};
                    {pstr_desc = Pstr_value (Nonrecursive, [
                         {
                           pvb_pat = Pat.var ~loc {txt = "query"; loc = loc};
                           pvb_expr = Exp.constant ~loc (Pconst_string (reprinted_query, delim));
                           pvb_attributes = [];
                           pvb_loc = loc;
                         }
                       ]); pstr_loc = loc};
                    {pstr_desc = Pstr_value (Nonrecursive, [
                         {
                           pvb_pat = { ppat_desc = Ppat_var { txt = "parse"; loc = loc }; ppat_loc = loc; ppat_attributes = []};
                           pvb_expr = { 
                             pexp_desc = Pexp_fun (Nolabel, None, {ppat_desc = Ppat_var {txt="value"; loc=loc}; ppat_loc = loc; ppat_attributes = []}, {
                                 pexp_loc = loc;
                                 pexp_attributes = [];
                                 pexp_desc = Unifier.unify_document_schema (add_loc delimLength loc) schema document;
                               });
                             pexp_loc = loc; pexp_attributes = []; 
                           };
                           pvb_attributes = [];
                           pvb_loc = loc;
                         }
                       ]); pstr_loc = loc};
                       {pstr_desc = Pstr_value (Recursive, Unifier.generate_encoders schema loc (add_loc delimLength loc) document); pstr_loc = loc};
                    {pstr_desc = Pstr_value (Nonrecursive, [
                         {
                           pvb_pat = { ppat_desc = Ppat_var { txt = "make"; loc = loc }; ppat_loc = loc; ppat_attributes = []};
                           pvb_expr = {
                             pexp_desc = Unifier.make_make_fun (add_loc delimLength loc) schema document;
                             pexp_loc = loc;
                             pexp_attributes = [];
                           };
                           pvb_attributes = [];
                           pvb_loc = loc;
                         }
                       ]); pstr_loc = loc};
                       
                  ])
          end
        | _ -> raise (Location.Error (
            Location.error ~loc "[%graphql] accepts a string, e.g. [%graphql {| { query |}]"
          ))
      end
    | other -> default_mapper.module_expr mapper other
  end in

  To_current.copy_mapper { default_mapper with module_expr }

let () = Compiler_libs.Ast_mapper.register "graphql" (fun _argv -> mapper ())
