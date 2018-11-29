module MyQuery = [%graphql {|
  mutation {
    mutationWithError {
      value {
        stringField
      }

      errors {
        field
        message
      }
    }
  }
|}]

let printed_query () =
  Alcotest.check Alcotest.int "string equal"
    (Str.search_forward (Str.regexp "^mutation") MyQuery.query 0)
    0

let tests = [
  "Printed query is a mutation", `Quick, printed_query;
]
