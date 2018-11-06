module My_query = [%graphql {|
  query ($arg: NonrecursiveInput!) {
    nonrecursiveInput(arg: $arg) # comment to test
  }
|}]
