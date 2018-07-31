module MyQuery: {
  type t = Js.t({
    .
    first: Js.t({
      .
      __typename: string,
      inner: option(Js.t({
        .
        __typename: string,
        inner: option(Js.t({
          .
          __typename: string,
          field: string
        })),
      })),
    }),
  });

  let make: unit => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });
  let makeWithVariables: Js.t({.}) => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });
  let query: string;
};