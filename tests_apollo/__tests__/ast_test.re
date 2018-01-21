open Jest;

[@bs.val] [@bs.module "graphql-tag"] external graphqlTag: string => Js.Json.t = "default";

module MyQuery = [%graphql {|
  {
    lists {
      nullableOfNullable
      nullableOfNonNullable
      nonNullableOfNullable
      nonNullableOfNonNullable
    }
  }|}];

let referenceQuery = graphqlTag({|
  {
    lists {
      nullableOfNullable
      nullableOfNonNullable
      nonNullableOfNullable
      nonNullableOfNonNullable
    }
  }|});

describe("AST Parsing", () => {
  open Expect;

  test("Equality", () => {
    expect(MyQuery.query) |> toEqual(referenceQuery);
  });
});