module MyQuery: {
  type t = Js.t({. recursiveInput: string});

  let make: (
    ~arg: Js.t({ . inner: option('a), enum: option([ | `FIRST | `SECOND | `THIRD ]), otherField: option(string) }) as 'a,
    unit) => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });
  let makeWithVariables: Js.t({
    .
    arg: Js.t({ . inner: option('a), enum: option([ | `FIRST | `SECOND | `THIRD ]), otherField: option(string) }) as 'a,
  }) => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });

  let query: string;
};