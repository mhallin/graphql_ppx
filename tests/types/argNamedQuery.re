module MyQuery = [%graphql {|
  query ($query: String!) {
    argNamedQuery(query: $query)
  }
|}];