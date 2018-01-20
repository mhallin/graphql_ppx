module MyQuery: {
  type t = Js.t {. scalarsInput : string};
  let make:
    nullableString::string? =>
    string::string =>
    nullableInt::int? =>
    int::int =>
    nullableFloat::float? =>
    float::float =>
    nullableBoolean::Js.boolean? =>
    boolean::Js.boolean =>
    nullableID::string? =>
    id::string =>
    unit =>
    Js.t {
      .
      parse : Js.Json.t => t, query : string, variables : Js.Json.t
    };
};