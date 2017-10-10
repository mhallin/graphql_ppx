module MyQuery: {
  let make:
    unit =>
    Js.t {
      .
      parse :
        Js.Json.t =>
        Js.t {
          .
          dogOrHuman : [
            | `Dog (Js.t {. name : string, barkVolume : float})
            | `Human (Js.t {. name : string})
          ]
        },
      query : string,
      variables : Js.Json.t
    };
};