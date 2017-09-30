module MyQuery: {
  let make:
    unit =>
    Js.t {
      .
      parse :
        Js.Json.t =>
        Js.t {
          .
          mutationWithError : [
            | `Value (Js.t {. stringField : string})
            | `Errors (array (Js.t {. field : [ | `FIRST | `SECOND | `THIRD], message : string}))
          ]
        },
      query : string,
      variables : Js.Json.t
    };
};