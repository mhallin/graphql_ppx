module MyQuery: {
  type t = Js.t {
    .
    variousScalars : Js.t {
      .
      nullableString : option string,
      string: string,
      nullableInt: option int,
      int: int,
      nullableFloat: option float,
      float: float,
      nullableBoolean: option Js.boolean,
      boolean: Js.boolean,
      nullableID: option string,
      id: string
    }
  };

  let make: unit => Js.t { . parse : Js.Json.t => t, query : string, variables : Js.Json.t };
  let makeWithVariables: Js.t {.} => Js.t { . parse : Js.Json.t => t, query : string, variables : Js.Json.t };
};