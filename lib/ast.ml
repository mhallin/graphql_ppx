open Source_pos

type typeRef =
  | Tr_named of string spanning
  | Tr_list of typeRef spanning
  | Tr_non_null_named of string spanning
  | Tr_non_null_list of typeRef spanning

type input_value =
  | Iv_null
  | Iv_int of int
  | Iv_float of float
  | Iv_string of string
  | Iv_boolean of bool
  | Iv_enum of string
  | Iv_variable of string
  | Iv_list of input_value spanning list
  | Iv_object of (string spanning * input_value spanning) list

type variable_definition = {
  vd_type: typeRef spanning;
  vd_default_value: input_value spanning option
}

type variable_definitions = (string spanning * variable_definition) list

type arguments = (string spanning * input_value spanning) list

type directive = { 
  d_name: string spanning;
  d_arguments: arguments spanning option;
}

type fragment_spread = {
  fs_name: string spanning;
  fs_directives: directive spanning list
}

type field = {
  fd_alias: string spanning option;
  fd_name: string spanning;
  fd_arguments: arguments spanning option;
  fd_directives: directive spanning list;
  fd_selection_set: selection list spanning option;
}

and inlineFragment = {
  if_type_condition: string spanning option;
  if_directives: directive spanning list;
  if_selection_set: selection list spanning;
}

and selection =
  | Field of field spanning
  | FragmentSpread of fragment_spread spanning
  | InlineFragment of inlineFragment spanning

type operationType = Query | Mutation

type operation = {
  o_type: operationType;
  o_name: string spanning option;
  o_variable_definitions: variable_definitions spanning option;
  o_directives: directive spanning list;
  o_selection_set: selection list spanning;
}

type fragment = {
  fg_name: string spanning;
  fg_type_condition: string spanning;
  fg_directives: directive spanning list;
  fg_selection_set: selection list spanning;
}

type definition =
  | Operation of operation spanning
  | Fragment of fragment spanning

type document = definition list
