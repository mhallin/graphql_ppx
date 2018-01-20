module MyQuery: {
  type t = Js.t {. variousScalars : Js.t {. string : int, int : string}};
  let make:
    unit =>
    Js.t {
      .
      parse : Js.Json.t => t,
      query : string,
      variables : Js.Json.t
    };
};