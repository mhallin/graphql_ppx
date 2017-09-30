module MyQuery = [%graphql
  {|
  query (
    $nullableOfNullable: [String],
    $nullableOfNonNullable: [String!],
    $nonNullableOfNullable: [String]!,
    $nonNullableOfNonNullable: [String!]!,
  ) {
    listsInput(arg: {
      nullableOfNullable: $nullableOfNullable,
      nullableOfNonNullable: $nullableOfNonNullable,
      nonNullableOfNullable: $nonNullableOfNullable,
      nonNullableOfNonNullable: $nonNullableOfNonNullable,
    })
  }
|}
];