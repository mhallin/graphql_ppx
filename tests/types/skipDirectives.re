module MyQuery = [%graphql
  {|
  query ($var: Boolean!) {
    v1: variousScalars {
      nullableString @skip(if: $var)
      string @skip(if: $var)
    }
    v2: variousScalars {
      nullableString @include(if: $var)
      string @include(if: $var)
    }
  }
|}
];