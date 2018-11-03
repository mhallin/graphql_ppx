module MyQuery = [%graphql {|
  query ($opt: CustomScalar, $req: CustomScalar!) {
    customScalarField(argOptional: $opt, argRequired: $req) {
      nullable
      nonNullable
    }
  }
|}];

Jest.(describe("Custom scalars", () => {
  open Expect;
  open! Expect.Operators;

  test("Encodes custom scalar variables as Json objects", () =>
    expect(MyQuery.make(
      ~opt=Js.Json.number(123.),
      ~req=Js.Json.number(456.),
      ())##variables)
    == Js.Json.parseExn({|
      {
        "opt": 123,
        "req": 456
      }
    |}));

  test("Encodes nullable scalar variables as optional Json objects", () =>
    expect(MyQuery.makeWithVariables({
      "opt": Some(Js.Json.number(123.)),
      "req": Js.Json.number(456.),
    })##variables)
    == Js.Json.parseExn({|
      {
        "opt": 123,
        "req": 456
      }
    |}));

  test("Decodes results to JSON", () =>
    expect(MyQuery.parse(Js.Json.parseExn({|{"customScalarField": { "nullable": 123, "nonNullable": 456 }}|})))
    == {"customScalarField": {
      "nullable": Some(Js.Json.number(123.)),
      "nonNullable": Js.Json.number(456.),
    }})
}));
