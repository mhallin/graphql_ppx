module MyQuery: {
  type t = Js.t({
    .
    mutationWithError : [
      | `Value(Js.t({. stringField: string}))
      | `Errors(array(Js.t({. field: [ | `FIRST | `SECOND | `THIRD], message: string})))
    ],
  });

  let make: unit => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });
  let makeWithVariables: Js.t({.}) => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });
  let query: string;
};