open Ast_402
open Parsetree
open Asttypes

open Graphql_ast
open Source_pos

let ser_optional serializer = function
  | None -> [%expr Obj.magic(Js.Undefined.empty)]
  | Some item -> serializer item

let ser_list_to_array serializer items = [%expr Js.Json.array([%e List.map serializer items |> Ast_helper.Exp.array])]

let ser_optional_list serializer = function
  | None | Some { item = [] } -> [%expr Js.Json.array([||])]
  | Some { item } -> ser_list_to_array serializer item

let ser_name { item = name } = 
  [%expr Js.Json.object_(Js.Dict.fromArray([|
    ("kind", Js.Json.string("Name"));
    ("value", Js.Json.string([%e Ast_helper.Exp.constant (Const_string (name, None))]));
  |]))]

let rec ser_type = function
  | { item = Tr_named name } -> [%expr Js.Json.object_(Js.Dict.fromArray([|
      ("kind", Js.Json.string("NamedType"));
      ("name", [%e ser_name name]);
    |]))]
  | { item = Tr_list inner } -> [%expr Js.Json.object_(Js.Dict.fromArray([|
      ("kind", Js.Json.string("ListType"));
      ("type", [%e ser_type inner]);
    |]))]
  | { item = Tr_non_null_named name } -> [%expr Js.Json.object_(Js.Dict.fromArray([|
      ("kind", Js.Json.string("NonNullType"));
      ("type", Js.Json.object_(Js.Dict.fromArray([|
        ("kind", Js.Json.string("NamedType"));
        ("name", [%e ser_name name])
      |])))
    |]))]
  | { item = Tr_non_null_list inner } -> [%expr Js.Json.object_(Js.Dict.fromArray([|
      ("kind", Js.Json.string("NonNullType"));
      ("type", Js.Json.object_(Js.Dict.fromArray([|
        ("kind", Js.Json.string("ListType"));
        ("type", [%e ser_type inner]);
      |])));
    |]))]

  

let rec ser_value = function
  | { item = Iv_null } -> [%expr Js.Json.object_(Js.Dict.fromArray([| ("kind", Js.Json.string("NullValue")) |]))]
  | { item = Iv_int i } -> [%expr Js.Json.object_(Js.Dict.fromArray([|
      ("kind", Js.Json.string("IntValue"));
      ("value", Js.Json.string([%e Ast_helper.Exp.constant (Const_string (string_of_int i, None))]));
    |]))]
  | { item = Iv_float f } -> [%expr Js.Json.object_(Js.Dict.fromArray([|
      ("kind", Js.Json.string("FloatValue"));
      ("value", Js.Json.string([%e Ast_helper.Exp.constant (Const_string (string_of_float f, None))]));
    |]))]
  | { item = Iv_string s } -> [%expr Js.Json.object_(Js.Dict.fromArray([|
      ("kind", Js.Json.string("StringValue"));
      ("value", Js.Json.string([%e Ast_helper.Exp.constant (Const_string (s, None))]));
    |]))]
  | { item = Iv_boolean b } -> [%expr Js.Json.object_(Js.Dict.fromArray([|
      ("kind", Js.Json.string("BooleanValue"));
      ("value", Js.Json.boolean([%e if b then [%expr Js.true_] else [%expr Js.false_]]));
    |]))]
  | { item = Iv_enum e } -> [%expr Js.Json.object_(Js.Dict.fromArray([|
      ("kind", Js.Json.string("EnumValue"));
      ("value", Js.Json.string([%e Ast_helper.Exp.constant (Const_string (e, None))]));
    |]))]
  | { item = Iv_variable v; span } -> [%expr Js.Json.object_(Js.Dict.fromArray([|
      ("kind", Js.Json.string("Variable"));
      ("name", [%e ser_name { item = v; span }])
    |]))]
  | { item = Iv_list l } -> [%expr Js.Json.object_(Js.Dict.fromArray([|
      ("kind", Js.Json.string("ListValue"));
      ("values", [%e ser_list_to_array ser_value l]);
    |]))]
  | { item = Iv_object o } -> [%expr Js.Json.object_(Js.Dict.fromArray([|
      ("kind", Js.Json.string("ObjectValue"));
      ("fields", [%e ser_list_to_array ser_object_field o])
    |]))]

and ser_object_field (k, v) = [%expr Js.Json.object_(Js.Dict.fromArray([|
    ("kind", Js.Json.string("ObjectField"));
    ("name", [%e ser_name k]);
    ("value", [%e ser_value v]);
  |]))]

let ser_argument (name, value) = [%expr Js.Json.object_(Js.Dict.fromArray([|
    ("kind", Js.Json.string("Argument"));
    ("name", [%e ser_name name]);
    ("value", [%e ser_value value]);
  |]))]

let ser_arguments args = ser_optional_list ser_argument args
  

let ser_variable_definition (name, def) =
  [%expr Js.Json.object_(Js.Dict.fromArray([|
    ("kind", Js.Json.string("VariableDefinition"));
    ("variable", Js.Json.object_(Js.Dict.fromArray([|
      ("kind", Js.Json.string("Variable"));
      ("name", [%e ser_name name]);
    |])));
    ("type", [%e ser_type def.vd_type]);
    ("defaultValue", [%e ser_optional ser_value def.vd_default_value]);
  |]))]

let ser_variable_definitions defs = ser_optional_list ser_variable_definition defs

let ser_directive {item} =
  [%expr Js.Json.object_(Js.Dict.fromArray([|
    ("kind", Js.Json.string("Directive"));
    ("name", [%e ser_name item.d_name]);
    ("arguments", [%e ser_arguments item.d_arguments]);
  |]))]

let ser_directives = ser_list_to_array ser_directive

let ser_type_condition tc = ser_type { item = Tr_named tc; span = tc.span }

let rec ser_selection_set sset = [%expr Js.Json.object_(Js.Dict.fromArray([|
    ("kind", Js.Json.string("SelectionSet"));
    ("selections", [%e ser_list_to_array ser_selection sset.item ]);
  |]))]

and ser_selection = function
  | Field { item } -> [%expr Js.Json.object_(Js.Dict.fromArray([|
      ("kind", Js.Json.string("Field"));
      ("alias", [%e ser_optional ser_name item.fd_alias]);
      ("name", [%e ser_name item.fd_name]);
      ("arguments", [%e ser_arguments item.fd_arguments]);
      ("directives", [%e ser_directives item.fd_directives]);
      ("selectionSet", [%e ser_optional ser_selection_set item.fd_selection_set])
    |]))]
  | FragmentSpread { item } -> [%expr Js.Json.object_(Js.Dict.fromArray([|
      ("kind", Js.Json.string("FragmentSpread"));
      ("name", [%e ser_name item.fs_name]);
      ("directives", [%e ser_directives item.fs_directives]);
    |]))]
  | InlineFragment { item } -> [%expr Js.Json.object_(Js.Dict.fromArray([|
      ("kind", Js.Json.string("InlineFragment"));
      ("typeCondition", [%e ser_optional ser_type_condition item.if_type_condition]);
      ("directives", [%e ser_directives item.if_directives]);
      ("selectionSet", [%e ser_selection_set item.if_selection_set]);
    |]))]

let ser_definition = function
  | Operation { item } -> [%expr Js.Json.object_(Js.Dict.fromArray([|
      ("kind", Js.Json.string("OperationDefinition"));
      ("name", [%e ser_optional ser_name item.o_name]);
      ("operation", Js.Json.string([%e match item.o_type with | Query -> [%expr "query"] | Mutation -> [%expr "mutation"]]));
      ("variableDefinitions", [%e ser_variable_definitions item.o_variable_definitions]);
      ("directives", [%e ser_directives item.o_directives]);
      ("selectionSet", [%e ser_selection_set item.o_selection_set]);
    |]))]
  | Fragment { item } -> [%expr Js.Json.object_(Js.Dict.fromArray([|
      ("kind", Js.Json.string("FragmentDefintion"));
      ("name", [%e ser_name item.fg_name]);
      ("typeCondition", [%e ser_type_condition item.fg_type_condition]);
      ("directives", [%e ser_directives item.fg_directives]);
      ("selectionSet", [%e ser_selection_set item.fg_selection_set]);
    |]))]

let def_end = function
  | Operation { span = (_, e) } | Fragment { span = (_, e) } -> e.index

let document_end = List.fold_left (fun end_ def -> max end_ (def_end def)) 0

let serialize_document source defs =
  [%expr Js.Json.object_(Js.Dict.fromArray([|
    ("kind", Js.Json.string("Document"));
    ("definitions", [%e ser_list_to_array ser_definition defs]);
    ("loc", Js.Json.object_(Js.Dict.fromArray([|
      ("start", Js.Json.number(0.0));
      ("end", Js.Json.number([%e Ast_helper.Exp.constant (Const_float (defs |> document_end |> string_of_int))]));
      ("source", Js.Json.string([%e Ast_helper.Exp.constant (Const_string (source, None))]));
      ("locationOffset", Js.Json.object_(Js.Dict.fromArray([|
        ("column", Js.Json.number(1.0));
        ("line", Js.Json.number(1.0));
      |])));
      ("name", Js.Json.string("GraphQL request"));
    |])));
  |]))]
