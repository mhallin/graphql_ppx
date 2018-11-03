module MyQuery = [%graphql {|
  {
    variousScalars {
      nullableString
      string
      nullableInt
      int
      nullableFloat
      float
      nullableBoolean
      boolean
      nullableID
      id
    }
  }
|}];

Jest.(describe("Scalars", () => {
  open Expect;
  open! Expect.Operators;

  test("Decodes non-null scalars", () =>
    expect(MyQuery.parse(Js.Json.parseExn({| {
      "variousScalars": {
        "nullableString": "a nullable string",
        "string": "a string",
        "nullableInt": 456,
        "int": 123,
        "nullableFloat": 678.5,
        "float": 1234.5,
        "nullableBoolean": false,
        "boolean": true,
        "nullableID": "a nullable ID",
        "id": "an ID"
      }
    } |}))) == {
      "variousScalars": {
        "nullableString": Some("a nullable string"),
        "string": "a string",
        "nullableInt": Some(456),
        "int": 123,
        "nullableFloat": Some(678.5),
        "float": 1234.5,
        "nullableBoolean": Some(false),
        "boolean": true,
        "nullableID": Some("a nullable ID"),
        "id": "an ID",
      }
    });

  test("Decodes null scalars", () =>
    expect(MyQuery.parse(Js.Json.parseExn({| {
      "variousScalars": {
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
      }
    } |}))) == {
      "variousScalars": {
        "nullableString": None,
        "string": "a string",
        "nullableInt": None,
        "int": 123,
        "nullableFloat": None,
        "float": 1234.5,
        "nullableBoolean": None,
        "boolean": true,
        "nullableID": None,
        "id": "an ID",
      }
    });

  test("Decodes omitted scalars", () =>
    expect(MyQuery.parse(Js.Json.parseExn({| {
      "variousScalars": {
        "string": "a string",
        "int": 123,
        "float": 1234.5,
        "boolean": true,
        "id": "an ID"
      }
    } |}))) == {
      "variousScalars": {
        "nullableString": None,
        "string": "a string",
        "nullableInt": None,
        "int": 123,
        "nullableFloat": None,
        "float": 1234.5,
        "nullableBoolean": None,
        "boolean": true,
        "nullableID": None,
        "id": "an ID",
      }
    });
}));
