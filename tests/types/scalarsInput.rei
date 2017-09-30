module MyQuery: {
  let make:
    arg::
      Js.t {
        ..
        nullableString : option string,
        string : string,
        nullableInt : option int,
        int : int,
        nullableFloat : option float,
        float : float,
        nullableBoolean : option Js.boolean,
        boolean : Js.boolean,
        nullableID : option string,
        id : string
      } =>
    unit =>
    Js.t {
      .
      parse : Js.Json.t => Js.t {. scalarsInput : string}, query : string, variables : Js.Json.t
    };
};