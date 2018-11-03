module MyQuery = [%graphql {|
  query ($arg: VariousScalarsInput!) {
    scalarsInput(arg: $arg)
  }
|}];

Jest.(describe("Scalars as arguments through an input object", () => {
  open Expect;
  open! Expect.Operators;

  test("Includes non-nulled arguments", () =>
    expect(MyQuery.make(
      ~arg={
        "nullableString": Some("a nullable string"),
        "string": "a string",
        "nullableInt": Some(456),
        "int": 123,
        "nullableFloat": Some(567.5),
        "float": 1234.5,
        "nullableBoolean": Some(false),
        "boolean": true,
        "nullableID": Some("a nullable ID"),
        "id": "an ID",
      },
      ())##variables)
      == Js.Json.parseExn({| {
        "arg": {
          "nullableString": "a nullable string",
          "string": "a string",
          "nullableInt": 456,
          "int": 123,
          "nullableFloat": 567.5,
          "float": 1234.5,
          "nullableBoolean": false,
          "boolean": true,
          "nullableID": "a nullable ID",
          "id": "an ID"
        }
      } |}));
}));
