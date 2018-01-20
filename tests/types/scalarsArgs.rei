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
    unit => Js.t { . parse : Js.Json.t => t, query : string, variables : Js.Json.t };

  let makeWithVariables: 
    Js.t {
      .
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
    } => Js.t { . parse : Js.Json.t => t, query : string, variables : Js.Json.t };
};
