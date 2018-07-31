module MyQuery = [%graphql {|
  query ($arg: SampleField!) {
    enumInput(arg: $arg)
  }
|}];

Jest.(describe("Enum arguments", () => {
  open Expect;
  open! Expect.Operators;

  test("Encodes enum arguments to strings", () =>
    expect(MyQuery.make(~arg=`FIRST, ())##variables)
      == Js.Json.parseExn({| { "arg": "FIRST" } |}))
}));
