module MyQuery: {
  type t = Js.t({
    .
    enumInput: string,
  });

  let make: (
    ~arg: [ | `FIRST | `SECOND | `THIRD],
    unit) => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });
  let makeWithVariables: Js.t({
    .
    arg: [ | `FIRST | `SECOND | `THIRD],
  }) => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });

  let query: string;
};