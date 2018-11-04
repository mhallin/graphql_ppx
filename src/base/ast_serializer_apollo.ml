open Graphql_ast
open Source_pos

let ser_optional serializer = function
  | None -> `Null
  | Some item -> serializer item

let ser_list_to_array serializer items = 
  `List (List.map serializer items)

let ser_optional_list serializer = function
  | None | Some { item = []; _ } -> `List []
  | Some { item; _ } -> ser_list_to_array serializer item

let ser_name { item = name; _ } = 
  `Assoc [
    "kind", `String "Name";
    "value", `String name;
  ]

let rec ser_type = function
  | { item = Tr_named name; _ } -> `Assoc [
      "kind", `String "NamedType";
      "name", ser_name name;
    ]
  | { item = Tr_list inner; _ } -> `Assoc [
      "kind", `String "ListType";
      "type", ser_type inner;
    ]
  | { item = Tr_non_null_named name; _ } -> `Assoc [
      "kind", `String "NonNullType";
      "type", `Assoc [
        "kind", `String "NamedType";
        "name", ser_name name;
      ]
    ]
  | { item = Tr_non_null_list inner; _ } -> `Assoc [
      "kind", `String "NonNullType";
      "type", `Assoc [
        "kind", `String "ListType";
        "type", ser_type inner;
      ];
    ]


let rec ser_value = function
  | { item = Iv_null; _ } -> `Assoc [ "kind", `String "NullValue" ]
  | { item = Iv_int i; _ } -> `Assoc [
      "kind", `String "IntValue";
      "value", `String (string_of_int i);
    ]
  | { item = Iv_float f; _ } -> `Assoc [
      "kind", `String "FloatValue";
      "value", `String (string_of_float f);
    ]
  | { item = Iv_string s; _ } -> `Assoc [
      "kind", `String "StringValue";
      "value", `String s;
    ]
  | { item = Iv_boolean b; _ } -> `Assoc [
      "kind", `String "BooleanValue";
      "value", `Bool b;
    ]
  | { item = Iv_enum e; _ } -> `Assoc [
      "kind", `String "EnumValue";
      "value", `String e;
    ]
  | { item = Iv_variable v; span; _ } -> `Assoc [
      "kind", `String "Variable";
      "name", ser_name { item = v; span };
    ]
  | { item = Iv_list l; _ } -> `Assoc [
      "kind", `String "ListValue";
      "values", ser_list_to_array ser_value l;
    ]
  | { item = Iv_object o; _ } -> `Assoc [
      "kind", `String "ObjectValue";
      "fields", ser_list_to_array ser_object_field o;
    ]

and ser_object_field (k, v) = `Assoc [
    "kind", `String "ObjectField";
    "name", ser_name k;
    "value", ser_value v;
  ]

let ser_argument (name, value) = `Assoc [
    "kind", `String "Argument";
    "name", ser_name name;
    "value", ser_value value;
  ]

let ser_arguments args = ser_optional_list ser_argument args


let ser_variable_definition (name, def) =
  `Assoc [
    "kind", `String "VariableDefinition";
    "variable", `Assoc [
      "kind", `String "Variable";
      "name", ser_name name;
    ];
    "type", ser_type def.vd_type;
    "defaultValue", ser_optional ser_value def.vd_default_value;
  ]

let ser_variable_definitions defs = ser_optional_list ser_variable_definition defs

let ser_directive {item; _} =
  `Assoc [
    "kind", `String "Directive";
    "name", ser_name item.d_name;
    "arguments", ser_arguments item.d_arguments;
  ]

let ser_directives = ser_list_to_array ser_directive

let ser_type_condition tc = ser_type { item = Tr_named tc; span = tc.span }

let rec ser_selection_set sset = `Assoc [
    "kind", `String "SelectionSet";
    "selections", ser_list_to_array ser_selection sset.item;
  ]

and ser_selection = function
  | Field { item; _ } -> `Assoc [
      "kind", `String "Field";
      "alias", ser_optional ser_name item.fd_alias;
      "name", ser_name item.fd_name;
      "arguments", ser_arguments item.fd_arguments;
      "directives", ser_directives item.fd_directives;
      "selectionSet", ser_optional ser_selection_set item.fd_selection_set;
    ]
  | FragmentSpread { item; _ } -> `Assoc [
      "kind", `String "FragmentSpread";
      "name", ser_name item.fs_name;
      "directives", ser_directives item.fs_directives;
    ]
  | InlineFragment { item; _ } -> `Assoc [
      "kind", `String "InlineFragment";
      "typeCondition", ser_optional ser_type_condition item.if_type_condition;
      "directives", ser_directives item.if_directives;
      "selectionSet", ser_selection_set item.if_selection_set;
    ]

let ser_definition = function
  | Operation { item; _ } -> `Assoc [
      "kind", `String "OperationDefinition";
      "name", ser_optional ser_name item.o_name;
      "operation", (match item.o_type with
          | Query -> `String "query"
          | Mutation -> `String "mutation"
          | Subscription -> `String "subscription");
      "variableDefinitions", ser_variable_definitions item.o_variable_definitions;
      "directives", ser_directives item.o_directives;
      "selectionSet", ser_selection_set item.o_selection_set;
    ]
  | Fragment { item; _ } -> `Assoc [
      "kind", `String "FragmentDefintion";
      "name", ser_name item.fg_name;
      "typeCondition", ser_type_condition item.fg_type_condition;
      "directives", ser_directives item.fg_directives;
      "selectionSet", ser_selection_set item.fg_selection_set;
    ]

let def_end = function
  | Operation { span = (_, e); _ } | Fragment { span = (_, e); _ } -> e.index

let document_end = List.fold_left (fun end_ def -> max end_ (def_end def)) 0

let serialize_document source defs =
  `Assoc [
    "kind", `String "Document";
    "definitions", ser_list_to_array ser_definition defs;
    "loc", `Assoc [
      "start", `Int 0;
      "end", `Int (defs |> document_end);
      "source", `StringExpr source;
      "locationOffset", `Assoc [
        "column", `Int 1;
        "line", `Int 1;
      ];
      "name", `String "GraphQL request";
    ];
  ]
