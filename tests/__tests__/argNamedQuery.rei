module MyQuery: {
  type t = Js.t({. argNamedQuery: int });

  let make: (~query: string, unit) => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });
  let makeWithVariables: Js.t({. query: string}) => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });
  let query: string;
};
