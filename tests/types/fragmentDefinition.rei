module Fragments: {
  module ListFragment: {
    type t = Js.t({.
      nullableOfNullable : option(array(option(string))),
      nullableOfNonNullable : option(array(string)),
    });

    let query: string;
    let name: string;
    let parse: Js.Json.t => t;
  };

  module Vars: {
    type t = Js.t({
      .
      argField: option(Js.t({
        .
        field: string
      })),
    });

    let query: string;
    let name: string;
    let parse: Js.Json.t => t;
  };
}
