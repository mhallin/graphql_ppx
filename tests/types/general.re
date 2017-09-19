module MyQuery = [%graphql
  {| mutation($name: String!, $email: String!, $password: String!) {
    signUp(email: $email, email: $email, password: $password) @bsVariant {
      user {
        name
      }

      errors {
        field
        message
      }
    }
  }
|}
];