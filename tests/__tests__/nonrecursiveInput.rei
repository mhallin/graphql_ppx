module MyQuery: {
    type t = Js.t({. nonrecursiveInput: string});
  
    let make: (
      ~arg: Js.t({ . enum: option([ | `FIRST | `SECOND | `THIRD ]), field: option(string) }),
      unit) => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });
    let makeWithVariables: Js.t({
      .
      arg: Js.t({ . enum: option([ | `FIRST | `SECOND | `THIRD ]), field: option(string) }) as 'a,
    }) => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });
  
    let query: string;
  };