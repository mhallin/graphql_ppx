module MyQuery = [%graphql {|
  query ($query: String!) {
    argNamedQuery(query: $query)
  }
|}];

Jest.(describe("Argument named 'query'", () => {
  open Expect;
  open! Expect.Operators;

  test("Serializes variables", () =>
    expect(MyQuery.make(~query="a query", ())##variables)
      == Js.Json.parseExn({|{"query": "a query"}|}));

  test("No name clash with the query field", () =>
    expect(MyQuery.make(~query="a query", ())##query) != "a query")
}))
