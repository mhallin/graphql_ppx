module Fragments = [%graphql {|
  fragment listFragment on Lists {
    nullableOfNullable
    nullableOfNonNullable
  }

  fragment vars on WithArgField {
    argField(arg1: $obj) {
      field
    }
  }
|}];

module MyQuery = [%graphql {|
  query {
    lists {
      ...Fragments.ListFragment
    }
  }
|}];

Jest.(describe("Fragment definition", () => {
  open Expect;
  open! Expect.Operators;

  test("Decodes the fragment", () =>
    expect(MyQuery.parse(Js.Json.parseExn({|{"lists": {"nullableOfNullable": ["a", null, "b"]}}|})))
    == {"lists": {"nullableOfNullable": Some([|Some("a"), None, Some("b")|]), "nullableOfNonNullable": None}});
}));
