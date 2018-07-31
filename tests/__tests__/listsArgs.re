module MyQuery = [%graphql
  {|
  query (
    $nullableOfNullable: [String],
    $nullableOfNonNullable: [String!],
    $nonNullableOfNullable: [String]!,
    $nonNullableOfNonNullable: [String!]!,
  ) {
    listsInput(arg: {
      nullableOfNullable: $nullableOfNullable,
      nullableOfNonNullable: $nullableOfNonNullable,
      nonNullableOfNullable: $nonNullableOfNullable,
      nonNullableOfNonNullable: $nonNullableOfNonNullable,
    })
  }
|}
];

Jest.(describe("Lists as query arguments", () => {
  open Expect;
  open! Expect.Operators;

  test("Allows you to omit nullable arguments", () =>
    expect(MyQuery.make(
      ~nonNullableOfNullable=[||],
      ~nonNullableOfNonNullable=[||],
      ())##variables)
    == Js.Json.parseExn({|
      {
        "nullableOfNullable": null,
        "nullableOfNonNullable": null,
        "nonNullableOfNullable": [],
        "nonNullableOfNonNullable": []
      }
    |}));

  test("Allows None in lists of nullable types", () =>
    expect(MyQuery.make(
      ~nullableOfNullable=[|Some("x"), None, Some("y")|],
      ~nonNullableOfNullable=[|Some("a"), None, Some("b")|],
      ~nonNullableOfNonNullable=[|"1", "2", "3"|],
      ())##variables)
    == Js.Json.parseExn({|
      {
        "nullableOfNullable": ["x", null, "y"],
        "nullableOfNonNullable": null,
        "nonNullableOfNullable": ["a", null, "b"],
        "nonNullableOfNonNullable": ["1", "2", "3"]
      }
    |}));
}));
