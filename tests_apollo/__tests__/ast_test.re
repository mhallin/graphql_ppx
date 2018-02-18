open Jest;

[@bs.val] [@bs.module "graphql-tag"] external graphqlTag: string => Js.Json.t = "default";

module Query1 = [%graphql {|
  {
    lists {
      nullableOfNullable
      nullableOfNonNullable
      nonNullableOfNullable
      nonNullableOfNonNullable
    }
  }|}];

let referenceQuery1 = graphqlTag({|
  {
    lists {
      nullableOfNullable
      nullableOfNonNullable
      nonNullableOfNullable
      nonNullableOfNonNullable
    }
  }|});

module Query2 = [%graphql {|
  query (
    $nullableString: String,
    $string: String!,
    $nullableInt: Int,
    $int: Int!,
    $nullableFloat: Float,
    $float: Float!,
    $nullableBoolean: Boolean,
    $boolean: Boolean!,
    $nullableID: ID,
    $id: ID!,
  ) {
    scalarsInput(arg: {
      nullableString: $nullableString,
      string: $string,
      nullableInt: $nullableInt,
      int: $int,
      nullableFloat: $nullableFloat,
      float: $float,
      nullableBoolean: $nullableBoolean,
      boolean: $boolean,
      nullableID: $nullableID,
      id: $id,
    })
  }
|}];

let referenceQuery2 = graphqlTag({|
  query (
    $nullableString: String,
    $string: String!,
    $nullableInt: Int,
    $int: Int!,
    $nullableFloat: Float,
    $float: Float!,
    $nullableBoolean: Boolean,
    $boolean: Boolean!,
    $nullableID: ID,
    $id: ID!,
  ) {
    scalarsInput(arg: {
      nullableString: $nullableString,
      string: $string,
      nullableInt: $nullableInt,
      int: $int,
      nullableFloat: $nullableFloat,
      float: $float,
      nullableBoolean: $nullableBoolean,
      boolean: $boolean,
      nullableID: $nullableID,
      id: $id,
    })
  }|});

describe("AST Parsing", () => {
  open Expect;

  test("Equality 1", () => {
    expect(Query1.query) |> toEqual(referenceQuery1);
  });

  test("Equality 2", () => {
    expect(Query2.query) |> toEqual(referenceQuery2);
  });
});