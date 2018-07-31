module MyQuery = [%graphql {|
  {
    dogOrHuman {
      ...on Dog {
        name
        barkVolume
      }

      ...on Human {
        name
      }
    }
  }
|}];

Jest.(describe("Union types", () => {
  open Expect;
  open! Expect.Operators;

  test("Decodes exhaustive query", () =>
    expect(MyQuery.parse(Js.Json.parseExn({| {
      "dogOrHuman": {
        "__typename": "Dog",
        "name": "Fido",
        "barkVolume": 123
      }
    } |}))) == {
      "dogOrHuman": `Dog({
        "name": "Fido",
        "barkVolume": 123.0,
      }),
    });
}));
