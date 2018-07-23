module Fragments: {
  module ListFragment: {
    type t = Js.t({
      .
      nullableOfNullable : option(array(option(string))),
      nullableOfNonNullable : option(array(string)),
    });

    let query: string;
    let name: string;
    let parse: Js.Json.t => t;
  };
};

module MyQuery: {
  type t = Js.t({.
    l1: Js.t({
      .
      nullableOfNullable : option(array(option(string))),
      nullableOfNonNullable : option(array(string)),
    }),
    l2: Js.t({
      .
      frag1: Js.t({
        .
        nullableOfNullable : option(array(option(string))),
        nullableOfNonNullable : option(array(string)),
      }),
      frag2: Js.t({
        .
        nullableOfNullable : option(array(option(string))),
        nullableOfNonNullable : option(array(string)),
      }),
    }),
  });

  let make: unit => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });
  let makeWithVariables: Js.t({.}) => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });
}