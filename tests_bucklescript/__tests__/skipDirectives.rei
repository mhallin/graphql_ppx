module MyQuery: {
  type t = Js.t({
    .
    v1: Js.t({. nullableString: option(string), string: option(string)}),
    v2: Js.t({. nullableString: option(string), string: option(string)}),
  });

  let make: (~var: bool, unit)
    => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });

  let makeWithVariables: Js.t({ . var: bool }) => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });

  let query: string;
};
