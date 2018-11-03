module MyQuery: {
  type t = Js.t({
    .
    simpleSubscription: [
      | `Dog(Js.t({ . name: string }))
      | `Human(Js.t({ . name: string }))
    ],
  });

  let make: unit => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });
  let makeWithVariables: Js.t({.}) => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });
  let query: string;
};