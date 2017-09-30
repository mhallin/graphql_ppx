module MyQuery: {
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
      parse : Js.Json.t => Js.t {. scalarsInput : string}, query : string, variables : Js.Json.t
    };
};