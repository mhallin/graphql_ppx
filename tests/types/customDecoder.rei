module MyQuery: {
  let make:
    unit =>
    Js.t {
      .
      parse : Js.Json.t => Js.t {. variousScalars : Js.t {. string : int, int : string}},
      query : string,
      variables : Js.Json.t
    };
};