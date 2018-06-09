module Fragments: {
  module ListFragment: {
    type t = Js.t({.
      nullableOfNullable : option(array(option(string))),
      nullableOfNonNullable : option(array(string)),
    });

    let make: Js.t({ .. }) => Js.Json.t => t;
    let query: string;
  };

  module Vars: {
    type t = Js.t({
      .
      argField: option(Js.t({
        .
        field: string
      })),
    });

    let make: (Js.t({ .. obj: unit }), Js.Json.t) => t;
    let query: string;
  };
}
