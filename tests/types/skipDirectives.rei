module MyQuery: {
  type t = Js.t {
    .
    v1 : Js.t {. nullableString : option string, string : option string},
    v2 : Js.t {. nullableString : option string, string : option string}
  };

  let make: var::Js.boolean => unit
    => Js.t { . parse : Js.Json.t => t, query : string, variables : Js.Json.t };

  let makeWithVariables: Js.t { . var: Js.boolean } => Js.t { . parse : Js.Json.t => t, query : string, variables : Js.Json.t };
};