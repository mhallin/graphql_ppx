module MyQuery = [%graphql {|
  query ($arg: ListsInput!) {
    listsInput(arg: $arg)
  }
|}];

Jest.(describe("Lists as query arguments through input object", () => {
  open Expect;
  open! Expect.Operators;

  test("Allows None in lists of nullable types", () =>
    expect(MyQuery.make(
      ~arg={
        "nullableOfNullable": Some([|Some("x"), None, Some("y")|]),
        "nullableOfNonNullable": None,
        "nonNullableOfNullable": [|Some("a"), None, Some("b")|],
        "nonNullableOfNonNullable": [|"1", "2", "3"|],
      },
      ())##variables)
    == Js.Json.parseExn({|
      {
        "arg": {
          "nullableOfNullable": ["x", null, "y"],
          "nullableOfNonNullable": null,
          "nonNullableOfNullable": ["a", null, "b"],
          "nonNullableOfNonNullable": ["1", "2", "3"]
        }
      }
    |}));
}));
