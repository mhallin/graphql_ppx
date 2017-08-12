module MyQuery = [%graphql {| mutation($name: String!, $email: String!, $password: String!) {
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
|}] 

let () =
  let query = MyQuery.make ~name:"my name" ~email:"test@example.com" ~password:"secret" () in
  let sample_response = Js.Json.parseExn {| {"signUp": {"user": {"name": "Sample name"}, "errors": null} } |} in
  let parse = query##parse in
  Js.Json.stringifyAny query##query |> Js.log2 "Query:";
  Js.log2 "Variables:" query##variables;
  (parse sample_response) |> Js.log2 "Sample response:"
