module MyQuery = [%graphql {|
  query (
    $nullableString: String,
    $string: String!,
    $nullableInt: Int,
    $int: Int!,
    $nullableFloat: Float,
    $float: Float!,
    $nullableBoolean: Boolean,
    $boolean: Boolean!,
    $nullableID: ID,
    $id: ID!,
  ) {
    scalarsInput(arg: {
      nullableString: $nullableString,
      string: $string,
      nullableInt: $nullableInt,
      int: $int,
      nullableFloat: $nullableFloat,
      float: $float,
      nullableBoolean: $nullableBoolean,
      boolean: $boolean,
      nullableID: $nullableID,
      id: $id,
    })
  }
|}];

Jest.(describe("Scalars as arguments", () => {
  open Expect;
  open! Expect.Operators;

  test("Allows you to omit nullable arguments", () =>
    expect(MyQuery.make(
      ~string="a string",
      ~int=123,
      ~float=1234.5,
      ~boolean=true,
      ~id="an ID",
      ())##variables)
      == Js.Json.parseExn({| {
        "nullableString": null,
        "string": "a string",
        "nullableInt": null,
        "int": 123,
        "nullableFloat": null,
        "float": 1234.5,
        "nullableBoolean": null,
        "boolean": true,
        "nullableID": null,
        "id": "an ID"
      } |}));

  test("Includes non-nulled arguments", () =>
    expect(MyQuery.make(
      ~nullableString="a nullable string",
      ~string="a string",
      ~nullableInt=456,
      ~int=123,
      ~nullableFloat=567.5,
      ~float=1234.5,
      ~nullableBoolean=false,
      ~boolean=true,
      ~nullableID="a nullable ID",
      ~id="an ID",
      ())##variables)
      == Js.Json.parseExn({| {
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
      } |}));

}));
