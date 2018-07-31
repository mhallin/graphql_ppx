module MyQuery = [%graphql {|
  mutation {
    mutationWithError {
      value {
        stringField
      }

      errors {
        field
        message
      }
    }
  }
|}];

Jest.(describe("Mutation", () => {
  open Expect;
  open! Expect.Operators;

  test("Printed query is a mutation", () =>
    expect(MyQuery.query |> Js.String.indexOf("mutation")) == 0);
}));
