module MyQuery = [%graphql
  {|
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
|}
];