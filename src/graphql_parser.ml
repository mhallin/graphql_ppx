open Result
open Source_pos

let map_ok f r = match r with
  | Ok x -> Ok (f x)
  | Error e -> Error e

type parser = {
  mutable tokens: Graphql_lexer.token spanning list;
}

type parseError =
  | Unexpected_token of Graphql_lexer.token
  | Unexpected_end_of_file
  | Lexer_error of Graphql_lexer.lexerError

let make tokens = { tokens = tokens }

let peek parser = List.hd parser.tokens

exception Internal_parse_error

let next parser = match parser.tokens with
  | [x] -> Error (replace x Unexpected_end_of_file)
  | x :: _ -> let () = parser.tokens <- List.tl parser.tokens in Ok x
  | _ -> raise Internal_parse_error

let expect parser token =
  match next parser with
  | Ok span when span.item = token -> Ok span
  | Ok span -> Error (replace span (Unexpected_token span.item))
  | x -> x

let expect_name parser =
  match next parser with
  | Ok ({ item = Graphql_lexer.Name name } as span) -> Ok (replace span name)
  | Ok ({ item = Graphql_lexer.End_of_file } as span) -> Error (replace span Unexpected_end_of_file)
  | Ok span -> Error (map (fun t -> Unexpected_token t) span)
  | Error e -> Error e

let skip parser token =
  match peek parser with
  | span when span.item = token -> map_ok (fun x -> Some x) (next parser)
  | span when span.item = Graphql_lexer.End_of_file -> Error (zero_width (start_pos span) Unexpected_end_of_file)
  | _ -> Ok None

let delimited_list parser opening sub_parser closing =
  match expect parser opening with
  | Error e -> Error e
  | Ok { span = (start_pos, _) } ->
    let rec scanner acc =
      match skip parser closing with
      | Ok (Some { span = (_, end_pos) }) -> Ok (start_end start_pos end_pos (List.rev acc))
      | _ ->
        match sub_parser parser with
          | Ok span -> scanner (span :: acc)
          | Error e -> Error e
    in
    scanner []

let delimited_nonempty_list parser opening sub_parser closing =
  match expect parser opening with
  | Error e -> Error e
  | Ok { span = (start_pos, _) } ->
    let rec scanner acc =
      match sub_parser parser with
      | Error e -> Error e
      | Ok span -> match skip parser closing with
        | Error e -> Error e
        | Ok Some { span = (_, end_pos) } -> Ok (start_end start_pos end_pos (List.rev (span :: acc)))
        | Ok None -> scanner (span :: acc)
  in
  scanner []
