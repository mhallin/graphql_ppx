module MyQuery = [%graphql
  {|
  query ($arg: VariousScalarsInput!) {
    scalarsInput(arg: $arg)
  }
|}
];