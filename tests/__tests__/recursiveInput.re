module MyQuery = [%graphql {|
  query ($arg: RecursiveInput!) {
    recursiveInput(arg: $arg)
  }
|}];

Jest.(describe("Recursive input types", () => {
  open Expect;
  open! Expect.Operators;

  test("Constructing a recursive input type", () =>
    expect(MyQuery.make(
      ~arg={
        "otherField": Some("test"),
        "inner": Some({
          "otherField": Some("inner"),
          "inner": None
        })
      },
      ())##variables)
      == Js.Json.parseExn({| {
        "arg": {
          "otherField": "test",
          "inner": {
            "otherField": "inner",
            "inner": null
          }
        }
      } |}));
}));
