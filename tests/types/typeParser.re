module NullableParser = [%graphql.parsetype "[SampleResult]"];

module NonnullParser = [%graphql.parsetype "[SampleResult!]!"];

module ScalarInput = [%graphql.parsetype "VariousScalarsInput!"];
