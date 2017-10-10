type scalars = {
  string,
  int
};

module MyQuery: {
  let make:
    unit =>
    Js.t {
      .
      parse : Js.Json.t => Js.t {. variousScalars : scalars}, query : string, variables : Js.Json.t
    };
};