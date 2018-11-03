module MyQuery = [%graphql {|
  mutation {
    mutationWithError @bsVariant {
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

Jest.(describe("Non-union variant through @bsVariant", () => {
  open Expect;
  open! Expect.Operators;

  test("Converts object into variant", () =>
    expect(MyQuery.parse(Js.Json.parseExn({| {
      "mutationWithError": {
        "value": {
          "stringField": "a string"
        }
      }
    } |}))) == {
      "mutationWithError": `Value({
        "stringField": "a string",
      }),
    });
}));
