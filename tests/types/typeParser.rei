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
