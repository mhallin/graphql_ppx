module NullableParser: {
  exception Graphql_error;
  let parse:
    Js.Json.t =>
    option (
      array (
        option (Js.t {. stringField : string, nested : option (Js.t {.})})
      )
    );
};

module NonnullParser: {
  exception Graphql_error;
  let parse:
    Js.Json.t =>
    array (Js.t {. stringField : string, nested : option (Js.t {.})});
};

module ScalarInput: {
  exception Graphql_error;
  let parse:
    Js.Json.t =>
      Js.t {. 
    nullableString: option(string),
    string: string,
    nullableInt: option(int),
    int: int,
    nullableFloat: option(float),
    float: float,
    nullableBoolean: option(Js.boolean),
    boolean: Js.boolean,
    nullableID: option(string),
    id: string
    };
};
