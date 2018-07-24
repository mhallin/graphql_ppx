module MyQuery = [%graphql {|
    query ($arg: NonrecursiveInput!) {
      nonrecursiveInput(arg: $arg)
    }
  |}];
  
  Jest.(describe("Recursive input types", () => {
    open Expect;
    open! Expect.Operators;
  
    test("Constructing a recursive input type", () =>
      expect(MyQuery.make(
        ~arg={
          "field": Some("test"),
          "enum": Some(`SECOND),
        },
        ())##variables)
        == Js.Json.parseExn({| {
          "arg": {
            "field": "test",
            "enum": "SECOND"
          }
        } |}));
  }));
  