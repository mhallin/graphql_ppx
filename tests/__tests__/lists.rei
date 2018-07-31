module MyQuery: {
  type t = Js.t({
    .
    lists : Js.t({
        .
        nullableOfNullable : option(array(option(string))),
        nullableOfNonNullable : option(array(string)),
        nonNullableOfNullable : array(option(string)),
        nonNullableOfNonNullable : array(string),
      })
  });

  let make: unit => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });
  let makeWithVariables: Js.t({.}) => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });
  let query: string;
};