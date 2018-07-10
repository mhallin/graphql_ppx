type exhaustive_flag = | Exhaustive | Nonexhaustive

type loc = Ast_402.Location.t

type t =
  | Res_nullable of loc * t
  | Res_array of loc * t
  | Res_id of loc
  | Res_string of loc
  | Res_int of loc
  | Res_float of loc
  | Res_boolean of loc
  | Res_raw_scalar of loc
  | Res_poly_enum of loc * Schema.enum_meta
  | Res_custom_decoder of loc * string * t
  | Res_record of loc * string * (string * t) list
  | Res_object of loc * string * (string * t) list
  | Res_poly_variant_selection_set of loc * string * (string * t) list
  | Res_poly_variant_union of loc * string * (string * t) list * exhaustive_flag
  | Res_solo_fragment_spread of loc * string
  | Res_error of loc * string

type mod_ =
  | Mod_fragment of string * (string list) * bool * Graphql_ast.fragment Source_pos.spanning * t
  | Mod_default_operation of Graphql_ast.variable_definitions Source_pos.spanning option * bool * Graphql_ast.operation Source_pos.spanning * t

let res_loc = function
  | Res_nullable (loc, _)
  | Res_array (loc, _)
  | Res_id loc | Res_string loc | Res_int loc | Res_float loc | Res_boolean loc | Res_raw_scalar loc
  | Res_poly_enum (loc, _)
  | Res_custom_decoder (loc, _, _)
  | Res_record (loc, _, _)
  | Res_object (loc, _, _)
  | Res_poly_variant_selection_set (loc, _, _)
  | Res_poly_variant_union (loc, _, _, _)
  | Res_solo_fragment_spread (loc, _)
  | Res_error (loc, _)
  -> loc

let can_be_absent_as_field = function
  | Res_nullable _ -> true
  | _ -> false
