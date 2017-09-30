module MyQuery: {
  let make:
    unit =>
    Js.t {
      .
      parse :
        Js.Json.t =>
        Js.t {
          .
          lists :
            Js.t {
              .
              nullableOfNullable : option (array (option string)),
              nullableOfNonNullable : option (array string),
              nonNullableOfNullable : array (option string),
              nonNullableOfNonNullable : array string
            }
        },
      query : string,
      variables : Js.Json.t
    };
};