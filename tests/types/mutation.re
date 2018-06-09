module MyQuery = [%graphql
  {| mutation {
    mutationWithError {
      value {
        stringField
      }

      errors {
        field
        message
      }
    }
  }
|}
];
