module MyQuery: {
  type t = Js.t({
    .
    dogOrHuman : [
      | `Dog(Js.t({. name : string, barkVolume : float}))
      | `Human(Js.t({. name : string}))
    ],
  });

  let make: unit => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });
  let makeWithVariables: Js.t({.}) => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });
  let query: string;
};