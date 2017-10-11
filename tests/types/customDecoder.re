module MyQuery = [%graphql
  {|
  {
    variousScalars {
      string @bsDecoder(fn: "int_of_string")
      int @bsDecoder(fn: "string_of_int")
    }
  }
|}
];