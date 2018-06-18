module MyQuery = [%graphql
  {| subscription {
    simpleSubscription {
      ...on Dog {
        name
      }
      ...on Human {
        name
      }
    }
  }
|}
];