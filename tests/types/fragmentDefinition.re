module Fragments = [%graphql {|
  fragment listFragment on Lists {
    nullableOfNullable
    nullableOfNonNullable
  }

  fragment vars on WithArgField {
    argField(arg1: $obj) {
      field
    }
  }
|}];
