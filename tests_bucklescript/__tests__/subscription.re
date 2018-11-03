module MyQuery = [%graphql {|
  subscription {
    simpleSubscription {
      ...on Dog {
        name
      }
      ...on Human {
        name
      }
    }
  }
|}];

Jest.(describe("Subscriptions", () => {
  open Expect;
  open! Expect.Operators;

  test("Printed query is a subscription", () =>
    expect(MyQuery.query |> Js.String.indexOf("subscription")) == 0);
}));
