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
  | Res_error of loc * string
