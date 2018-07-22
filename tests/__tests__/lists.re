module MyQuery = [%graphql {|
  {
    lists {
      nullableOfNullable
      nullableOfNonNullable
      nonNullableOfNullable
      nonNullableOfNonNullable
    }
  }
|}];

Jest.(describe("Lists", () => {
  open Expect;
  open! Expect.Operators;

  test("Null in nullable lists", () =>
    expect(MyQuery.parse(Js.Json.parseExn({|{"lists": {"nullableOfNullable": [null, "123"], "nonNullableOfNullable": [null, "123"], "nonNullableOfNonNullable": ["a", "b"]}}|})))
    == {"lists": {
      "nullableOfNullable": Some([|None, Some("123")|]),
      "nullableOfNonNullable": None,
      "nonNullableOfNullable": [|None, Some("123")|],
      "nonNullableOfNonNullable": [|"a", "b"|]
    }})
}));
