module MyQuery: {
  let make:
    name::string =>
    email::string =>
    password::string =>
    unit =>
    Js.t {
      .
      parse :
        Js.Json.t =>
        Js.t {
          .
          signUp : [>
            | `Errors (array (Js.t {. field : [ | `EMAIL | `NAME | `PASSWORD], message : string}))
            | `User (Js.t {. name : string})
          ]
        },
      query : string,
      variables : Js.Json.t
    };
};
