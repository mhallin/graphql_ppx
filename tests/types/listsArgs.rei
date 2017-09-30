module MyQuery: {
  let make:
    nullableOfNullable::array (option string)? =>
    nullableOfNonNullable::array string? =>
    nonNullableOfNullable::array (option string) =>
    nonNullableOfNonNullable::array string =>
    unit =>
    Js.t {
      .
      parse : Js.Json.t => Js.t {. listsInput : string}, query : string, variables : Js.Json.t
    };
};