type record = {
  f1: string,
  f2: string
};

module MyQuery: {
  let make:
    unit =>
    Js.t {
      .
      parse :
        Js.Json.t =>
        Js.t {
          .
          first : Js.t {. inner : option (Js.t {. inner : option (Js.t {. field : string})})},
          second : Js.t {. inner : option (Js.t {. inner : option record})}
        },
      query : string,
      variables : Js.Json.t
    };
};