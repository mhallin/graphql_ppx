module MyMutation = [%graphql {|
  mutation($field: String) {
    mutationWithOptionalArgument(field: $field) {
      value {
        stringField
      }
    }
  }
|}];

Jest.(describe("Mutation", () => {
  open Expect;
  open! Expect.Operators;

  test("Omits not provided optional arguments", () => {
    let variables = MyMutation.make(())##variables;
    expect(variables) == Js.Json.parseExn({|{}|});
  });
}));
