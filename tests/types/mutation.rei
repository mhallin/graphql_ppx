module MyQuery: {
  let make:
    unit =>
    Js.t {
      .
      parse :
        Js.Json.t =>
        Js.t {
          .
          mutationWithError :
            Js.t {
              .
              value : option (Js.t {. stringField : string}),
              errors :
                option (array (Js.t {. field : [ | `FIRST | `SECOND | `THIRD], message : string}))
            }
        },
      query : string,
      variables : Js.Json.t
    };
};