module MyQuery = [%graphql {|
  {
    first: nestedObject {
      __typename
      inner {
        __typename
        inner {
          __typename
          field
        }
      }
    }
  }
|}];

Jest.(describe("Typename as implicit field", () => {
  open Expect;
  open! Expect.Operators;

  test("Decodes typename as a non-nullable string", () =>
    expect(MyQuery.parse(Js.Json.parseExn({|
      {"first": {"__typename": "NestedObject", "inner": null}}
    |}))) == {"first": {"__typename": "NestedObject", "inner": None}});
}));
