module MyQuery: {
  type t = Js.t({
    .
    mutationWithError: Js.t({ .
      value: option(Js.t({. stringField: string})),
      errors: option(array(Js.t({. field: [ | `FIRST | `SECOND | `THIRD], message: string}))),
    }),
  });

  let make: unit => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });
  let makeWithVariables: Js.t({.}) => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });

  let query: string;
};