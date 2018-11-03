module MyQuery = [%graphql {|
  {
    dogOrHuman {
      ...on Dog {
        name
        barkVolume
      }
    }
  }
|}];

Jest.(describe("Union types", () => {
  open Expect;
  open! Expect.Operators;

  test("Decodes non-exhaustive query", () =>
    expect(MyQuery.parse(Js.Json.parseExn({| {
      "dogOrHuman": {
        "__typename": "Human",
        "name": "Max"
      }
    } |}))) == {
      "dogOrHuman": `Nonexhaustive,
    });
}));
