module Fragments = [%graphql {|
  fragment listFragment on Lists {
    nullableOfNullable
    nullableOfNonNullable
  }
|}];

module MyQuery = [%graphql {|
  query {
    l1: lists {
      ...Fragments.ListFragment
    }

    l2: lists {
      ...Fragments.ListFragment @bsField(name: "frag1")
      ...Fragments.ListFragment @bsField(name: "frag2")
    }
  }
|}];

Jest.(describe("Fragment definition", () => {
  open Expect;
  open! Expect.Operators;

  test("Decodes the fragment", () =>
    expect(MyQuery.parse(Js.Json.parseExn({|
      {
        "l1": {"nullableOfNullable": ["a", null, "b"]},
        "l2": {"nullableOfNullable": ["a", null, "b"]}
      }|})))
    == {
      "l1": {
        "nullableOfNullable": Some([|Some("a"), None, Some("b")|]),
        "nullableOfNonNullable": None,
      },
      "l2": {
        "frag1": {
          "nullableOfNullable": Some([|Some("a"), None, Some("b")|]),
          "nullableOfNonNullable": None,
        },
        "frag2": {
          "nullableOfNullable": Some([|Some("a"), None, Some("b")|]),
          "nullableOfNonNullable": None,
        },
      },
    });
}));
