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
        "enum": None,
        "inner": Some({
          "otherField": Some("inner"),
          "inner": None,
          "enum": Some(`SECOND),
        })
      },
      ())##variables)
      == Js.Json.parseExn({| {
        "arg": {
          "otherField": "test",
          "enum": null,
          "inner": {
            "otherField": "inner",
            "inner": null,
            "enum": "SECOND"
          }
        }
      } |}));
}));
