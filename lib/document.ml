open Result
open Ast

open Gql_parser
open Source_pos

let last l = match List.length l with
  | 0 -> None
  | n -> Some (List.nth l n)

let selection_end_pos s = match s with
  | Ast.Field { span = _, end_pos } -> end_pos
  | Ast.FragmentSpread { span = _, end_pos } -> end_pos
  | Ast.InlineFragment { span = _, end_pos } -> end_pos

let parse_argument parser = 
  match expect_name parser with
  | Error e -> Error e
  | Ok name -> match expect parser Gql_lexer.Colon with
    | Error e -> Error e
    | Ok _ -> match Value.parse_value_literal false parser with
      | Error e -> Error e
      | Ok value -> Ok (name, value)

let parse_arguments parser = match peek parser with
  | { item = Gql_lexer.Paren_open } -> map_ok (fun args -> Some args)
    (delimited_nonempty_list parser Gql_lexer.Paren_open parse_argument Gql_lexer.Paren_close)
  | _ -> Ok None

let parse_directive parser = match expect parser Gql_lexer.At with
  | Error e -> Error e
  | Ok { span = start_pos, _ } -> match expect_name parser with
    | Error e -> Error e
    | Ok name_span -> match parse_arguments parser with
      | Error e -> Error e
      | Ok Some arguments -> Ok (
        start_end
        start_pos
        (match arguments.item with | hd :: _ -> hd |> snd |> end_pos | [] -> name_span |> end_pos)
        { d_name = name_span; d_arguments = Some arguments }
      )
      | Ok None -> Ok (
        start_end start_pos (end_pos name_span) { d_name = name_span; d_arguments = None }
      )

let parse_directives parser = match peek parser with
  | { item = Gql_lexer.At } -> 
    let rec scanner acc = match peek parser with
      | { item = Gql_lexer.At } -> begin match parse_directive parser with
        | Error e -> Error e
        | Ok directive -> scanner (directive :: acc)
        end
      | _ -> Ok (List.rev acc)
    in
    scanner []
  | span -> Ok []

let rec parse_type parser = match skip parser Gql_lexer.Bracket_open with
  | Error e -> Error e
  | Ok (Some { span = start_pos, _ }) -> begin match parse_type parser with
    | Error e -> Error e
    | Ok inner_type -> match expect parser Gql_lexer.Bracket_close with
      | Error e -> Error e
      | Ok { span = _, (end_pos as bracket_end_pos) } -> match peek parser with
        | { item = Gql_lexer.Exclamation_mark; span = _, end_pos } ->
          let _ = next parser in
          Ok (start_end start_pos end_pos (Tr_non_null_list inner_type))
        | _ -> Ok (start_end start_pos bracket_end_pos (Tr_list inner_type))
    end
  | Ok None -> match expect_name parser with
    | Error e -> Error e
    | Ok name_span -> match peek parser with
      | { item = Gql_lexer.Exclamation_mark; span = _, end_pos } ->
        let _ = next parser in
        Ok (start_end (start_pos name_span) end_pos (Tr_non_null_named name_span))
      | _ -> Ok (replace name_span (Tr_named name_span))

let parse_variable_definition parser = match expect parser Gql_lexer.Dollar with
  | Error e -> Error e
  | Ok { span = start_pos, _ } -> match expect_name parser with
    | Error e -> Error e
    | Ok name_span -> match expect parser Gql_lexer.Colon with
      | Error e -> Error e
      | Ok _ -> match parse_type parser with
        | Error e -> Error e
        | Ok ty -> match skip parser Gql_lexer.Equals with
          | Error e -> Error e
          | Ok Some _ -> begin match Value.parse_value_literal true parser with
            | Error e -> Error e
            | Ok default_value -> Ok (start_end
              start_pos
              (end_pos default_value)
              (
                start_end start_pos (end_pos name_span) name_span.item,
                { vd_type = ty; vd_default_value = Some default_value }
              )
            )
            end
          | Ok None -> Ok (start_end
            start_pos
            (end_pos ty)
            (
              start_end start_pos (end_pos name_span) name_span.item,
              { vd_type = ty; vd_default_value = None}
            ))


let parse_variable_definitions parser = match peek parser with
  | { item = Gql_lexer.Paren_open } ->
    map_ok (fun span -> Some (map (fun items -> List.map (fun s -> s.item) items) span))
      (delimited_nonempty_list parser Gql_lexer.Paren_open parse_variable_definition Gql_lexer.Paren_close)
  | _ -> Ok None


let rec parse_selection_set parser =
  delimited_nonempty_list parser Gql_lexer.Curly_open parse_selection Gql_lexer.Curly_close

and parse_optional_selection_set parser = match peek parser with
  | { item = Gql_lexer.Curly_open } -> map_ok (fun x -> Some x) (parse_selection_set parser)
  | span -> Ok None


and parse_field parser =
  let parse_rest alias name = match parse_arguments parser with
    | Error e -> Error e
    | Ok arguments -> match parse_directives parser with
      | Error e -> Error e
      | Ok directives -> match parse_optional_selection_set parser with
        | Error e -> Error e
        | Ok selection_set -> Ok (start_end
          (match alias with | Some { span = start_pos, _ } -> start_pos | None -> name |> start_pos)
          (match selection_set with | Some { span = _, end_pos } -> end_pos | None ->
            match last directives with | Some { span = _, end_pos } -> end_pos | None ->
              match arguments with | Some { span = _, end_pos } -> end_pos | None -> name |> end_pos)
          {
            fd_alias = alias;
            fd_name = name;
            fd_arguments = arguments;
            fd_directives = directives;
            fd_selection_set = selection_set;
          }
        )
  in 
  match expect_name parser with 
  | Error e -> Error e
  | Ok alias_or_name -> match skip parser Gql_lexer.Colon with
    | Error e -> Error e
    | Ok None -> parse_rest None alias_or_name
    | Ok Some _ -> match expect_name parser with
      | Error e -> Error e
      | Ok name -> parse_rest (Some alias_or_name) name

and parse_fragment parser =
  match expect parser Gql_lexer.Ellipsis with
  | Error e -> Error e
  | Ok { span = start_pos, _ } -> match peek parser with
    | { item = Gql_lexer.Name "on" } -> begin let _ = next parser in
      match expect_name parser with
      | Error e -> Error e
      | Ok name_span -> match parse_directives parser with
        | Error e -> Error e
        | Ok directives -> match parse_selection_set parser with
          | Error e -> Error e
          | Ok selection_set -> Ok (Ast.InlineFragment (start_end 
            start_pos
            (end_pos selection_set)
            { 
              if_type_condition = Some name_span;
              if_directives = directives;
              if_selection_set = selection_set;
            }
          ))
      end
    | { item = Gql_lexer.Curly_open } -> begin match parse_selection_set parser with
      | Error e -> Error e
      | Ok selection_set -> Ok (Ast.InlineFragment (start_end
        start_pos
        (end_pos selection_set)
        {
          if_type_condition = None;
          if_directives = [];
          if_selection_set = selection_set;
        }))
      end
    | { item = Gql_lexer.Name _ } -> begin match expect_name parser with
      | Error e -> Error e
      | Ok name_span -> match parse_directives parser with
        | Error e -> Error e
        | Ok directives -> Ok (Ast.FragmentSpread (start_end
          start_pos
          (match last directives with | Some s -> (end_pos s) | None -> (end_pos name_span))
          {
            fs_name = name_span;
            fs_directives = directives;
          }
        ))
      end
    | { item = Gql_lexer.At } -> begin match parse_directives parser with
      | Error e -> Error e
      | Ok directives -> match parse_selection_set parser with 
        | Error e -> Error e
        | Ok selection_set -> Ok (Ast.InlineFragment (start_end
          start_pos
          (end_pos selection_set)
          {
            if_type_condition = None;
            if_directives = directives;
            if_selection_set = selection_set;
          }
        ))
      end
    | _ -> match next parser with
      | Error e -> Error e
      | Ok span -> Error (map (fun t -> Unexpected_token t) span)

and parse_selection parser =
  match peek parser with
  | { item = Gql_lexer.Ellipsis } -> parse_fragment parser
  | _ -> map_ok (fun (span: Ast.field spanning) -> Ast.Field span) (parse_field parser)

let parse_operation_type parser = match next parser with
  | Error e -> Error e
  | Ok ({ item = Gql_lexer.Name "query" } as span) -> Ok (replace span Ast.Query)
  | Ok ({ item = Gql_lexer.Name "mutation"} as span) -> Ok (replace span Ast.Mutation)
  | Ok span -> Error (map (fun t -> Unexpected_token t) span)

let parse_operation_definition parser =
  match peek parser with
  | { item = Gql_lexer.Curly_open } -> begin match parse_selection_set parser with
    | Error e -> Error e
    | Ok span -> Ok (replace span {
        o_type = Query;
        o_name = None;
        o_variable_definitions = None;
        o_directives = [];
        o_selection_set = span
      })
    end
  | { span = start_pos, _ } -> 
    let rec parse_rest operation_type name = match parse_variable_definitions parser with
      | Error e -> Error e
      | Ok variable_definitions -> match parse_directives parser with
        | Error e -> Error e
        | Ok directives -> match parse_selection_set parser with
          | Error e -> Error e
          | Ok selection_set -> Ok (start_end
            start_pos
            (end_pos selection_set)
            {
              o_type = operation_type;
              o_name = name;
              o_variable_definitions = variable_definitions;
              o_directives = directives;
              o_selection_set = selection_set;
            }
          )
    in
    match parse_operation_type parser with
    | Error e -> Error e
    | Ok operation_type -> match peek parser with
      | { item = Gql_lexer.Name _ } -> begin match expect_name parser with
        | Error e -> Error e
        | Ok name_span -> parse_rest operation_type.item (Some name_span)
        end
      | _ -> parse_rest operation_type.item None


let parse_fragment_definition parser = match expect parser (Gql_lexer.Name "fragment") with
  | Error e -> Error e
  | Ok { span = start_pos, _ } -> match expect_name parser with
    | Error e -> Error e
    | Ok ({ item = "on" } as span) -> Error (replace span (Unexpected_token (Gql_lexer.Name "on")))
    | Ok name_span -> match expect parser (Gql_lexer.Name "on") with
      | Error e -> Error e
      | Ok _ -> match expect_name parser with
        | Error e -> Error e
        | Ok type_cond -> match parse_directives parser with
          | Error e -> Error e
          | Ok directives -> match parse_selection_set parser with
            | Error e -> Error e
            | Ok selection_set -> Ok (start_end
              start_pos
              (end_pos selection_set)
              {
                fg_name = name_span;
                fg_type_condition = type_cond;
                fg_directives = directives;
                fg_selection_set = selection_set;
              })

let parse_definition parser =
  match peek parser with
  | { item = Gql_lexer.Curly_open } | {item = Gql_lexer.Name "query" } | { item = Gql_lexer.Name "mutation"} ->
    map_ok (fun def -> Ast.Operation def) (parse_operation_definition parser)
  | { item = Gql_lexer.Name "fragment" } ->
    map_ok (fun def -> Ast.Fragment def) (parse_fragment_definition parser)
  | span -> Error (map (fun t -> Unexpected_token t) span)

let parse_document parser =
  let rec scanner acc = match parse_definition parser with
    | Error e -> Error e
    | Ok def -> match peek parser with
      | { item = Gql_lexer.End_of_file } -> Ok (List.rev (def :: acc))
      | _ -> scanner (def :: acc)
  in
  scanner []
