type scalars = {
  string: string,
  int: int
};

module MyQuery = [%graphql
  {|
  {
    variousScalars @bsRecord {
      string
      int
    }
  }
|}
];
