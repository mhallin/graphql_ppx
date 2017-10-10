module MyQuery = [%graphql
  {|
    {
      dogOrHuman {
        ...on Dog {
          name
          barkVolume
        }
      }
    }
  |}
];