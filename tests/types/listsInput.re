module MyQuery = [%graphql
  {|
  query ($arg: ListsInput!) {
    listsInput(arg: $arg)
  }
|}
];