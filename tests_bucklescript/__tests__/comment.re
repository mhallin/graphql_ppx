module MyQuery = [%graphql {|
    query ($arg: NonrecursiveInput!) {
      nonrecursiveInput(arg: $arg) # comment to test
    }
  |}];
  
  Jest.(describe("Comment in query", () => {
    open Expect;
    open! Expect.Operators;
  
    test("Constructs with comment in query", () =>
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
  
