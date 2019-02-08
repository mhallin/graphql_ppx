module MyMutation: {
  type t = Js.t({
    .
    mutationWithOptionalArgument: Js.t({ .
      value: option(Js.t({. stringField: string})),
    }),
  });

  let make: (~field: string=?, unit) => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });
  let makeWithVariables: {. "field": option(string) } => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });

  let query: string;
};