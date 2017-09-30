module MyQuery = [%graphql
  {| mutation {
    mutationWithError @bsVariant {
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