type typeRef =
  | Tr_named of string SourcePos.spanning
  | Tr_list of typeRef SourcePos.spanning
  | Tr_non_null_named of string SourcePos.spanning
  | Tr_non_null_list of typeRef SourcePos.spanning

type input_value =
  | Iv_null
  | Iv_int of int
  | Iv_float of float
  | Iv_string of string
  | Iv_boolean of bool
  | Iv_enum of string
  | Iv_variable of string
  | Iv_list of input_value SourcePos.spanning list
  | Iv_object of (string SourcePos.spanning * input_value SourcePos.spanning) list

type variable_definition = {
  vd_type: typeRef SourcePos.spanning;
  vd_default_value: input_value SourcePos.spanning option
}

type variable_definitions = (string SourcePos.spanning * variable_definition) list

type arguments = (string SourcePos.spanning * input_value SourcePos.spanning) list

type directive = { 
  d_name: string SourcePos.spanning;
  d_arguments: arguments SourcePos.spanning option;
}

type fragment_spread = {
  fs_name: string SourcePos.spanning;
  fs_directives: directive SourcePos.spanning list
}

type field = {
  fd_alias: string SourcePos.spanning option;
  fd_name: string SourcePos.spanning;
  fd_arguments: arguments SourcePos.spanning option;
  fd_directives: directive SourcePos.spanning list;
  fd_selection_set: selection list SourcePos.spanning option;
}

and inlineFragment = {
  if_type_condition: string SourcePos.spanning option;
  if_directives: directive SourcePos.spanning list;
  if_selection_set: selection list SourcePos.spanning;
}

and selection =
  | Field of field SourcePos.spanning
  | FragmentSpread of fragment_spread SourcePos.spanning
  | InlineFragment of inlineFragment SourcePos.spanning

type operationType = Query | Mutation

type operation = {
  o_type: operationType;
  o_name: string SourcePos.spanning option;
  o_variable_definitions: variable_definitions SourcePos.spanning option;
  o_directives: directive SourcePos.spanning list;
  o_selection_set: selection list SourcePos.spanning;
}

type fragment = {
  fg_name: string SourcePos.spanning;
  fg_type_condition: string SourcePos.spanning;
  fg_directives: directive SourcePos.spanning list;
  fg_selection_set: selection list SourcePos.spanning;
}

type definition =
  | Operation of operation SourcePos.spanning
  | Fragment of fragment SourcePos.spanning

type document = definition list
