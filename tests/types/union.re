module MyQuery = [%graphql
  {|
    {
      dogOrHuman {
        ...on Dog {
          name
          barkVolume
        }

        ...on Human {
          name
        }
      }
    }
  |}
];