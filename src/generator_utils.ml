open Ast_402
open Parsetree
open Asttypes

type error_marker = {
  mutable has_error: bool
}

let raise_error_with_loc loc message =
  raise (Location.Error (
      Location.error ~loc message
    ))

let raise_error map_loc span message =
  raise_error_with_loc (map_loc span) message

let some_or o d = match o with
  | Some v -> v
  | None -> d

let capitalize_ascii = String.capitalize
let uncapitalize_ascii = String.uncapitalize

type output_mode =
  | String
  | Apollo_AST

type output_config = {
  map_loc: Source_pos.source_position * Source_pos.source_position -> Result_structure.loc;
  delimiter: string option;
  output_mode: output_mode;
  verbose_error_handling: bool;
  schema: Schema.schema;
  full_document: Graphql_ast.document;
}

let filter_map f l =
  let rec loop acc = function
  | [] -> List.rev acc
  | head :: tail -> match f head with
    | None -> loop acc tail
    | Some v -> loop (v :: acc) tail
  in loop [] l
