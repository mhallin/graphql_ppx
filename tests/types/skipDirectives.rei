module MyQuery: {
  let make:
    var::Js.boolean =>
    unit =>
    Js.t {
      .
      parse :
        Js.Json.t =>
        Js.t {
          .
          v1 : Js.t {. nullableString : option string, string : option string},
          v2 : Js.t {. nullableString : option string, string : option string}
        },
      query : string,
      variables : Js.Json.t
    };
};