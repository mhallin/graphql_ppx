open Result

open Gql_parser
open SourcePos

open Ast

let map_next_token parser f = match next parser with
  | Ok item -> Ok (map f item)
  | e -> e

let replace_next_token parser t = match next parser with
  | Ok item -> Ok (replace item t)
  | Error e -> Error e

let map_next_token_with_error parser f = match next parser with
  | Ok item -> Error (map f item)
  | Error e -> Error e

let parse_variable_literal parser =
  match expect parser Gql_lexer.Dollar with
  | Error e -> Error e
  | Ok { span = (start_pos, _) } -> match expect_name parser with
    | Error e -> Error e
    | Ok { item; span = (_, end_pos) } -> Ok (start_end start_pos end_pos (Iv_variable item))

let rec parse_value_literal is_const parser = match peek parser with
  | { item = Gql_lexer.Bracket_open } -> parse_list_literal is_const parser
  | { item = Gql_lexer.Curly_open } -> parse_object_literal is_const parser
  | { item = Gql_lexer.Dollar } when not is_const -> parse_variable_literal parser
  | { item = Gql_lexer.Int i } -> replace_next_token parser (Iv_int i)
  | { item = Gql_lexer.Float f } -> replace_next_token parser (Iv_float f)
  | { item = Gql_lexer.String s } -> replace_next_token parser (Iv_string s)
  | { item = Gql_lexer.Name "true" } -> replace_next_token parser (Iv_boolean true)
  | { item = Gql_lexer.Name "false" } -> replace_next_token parser (Iv_boolean false)
  | { item = Gql_lexer.Name "null" } -> replace_next_token parser (Iv_null)
  | { item = Gql_lexer.Name name } -> replace_next_token parser (Iv_enum name)
  | _ -> map_next_token_with_error parser (fun t -> Unexpected_token t)

and parse_list_literal is_const parser =
  map_ok (fun span -> map (fun items -> Iv_list items) span)
    (delimited_list parser Gql_lexer.Bracket_open (parse_value_literal is_const) Gql_lexer.Bracket_close)

and parse_object_literal is_const parser =
  map_ok (fun span -> map (fun items -> Iv_object items) span)
    (delimited_list parser Gql_lexer.Curly_open (parse_object_field is_const) Gql_lexer.Curly_close)

and parse_object_field is_const parser =
  match expect_name parser with
  | Error e -> Error e
  | Ok key -> match expect parser Gql_lexer.Colon with
    | Error e -> Error e
    | Ok _ -> match parse_value_literal is_const parser with
      | Error e -> Error e
      | Ok value -> Ok (key, value)
