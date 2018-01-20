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

module MyNonexhaustiveQuery = [%graphql
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
