open Test_shared

module MyQuery = [%graphql {|
  query ($arg: RecursiveInput!) {
    recursiveInput(arg: $arg)
  }
|}]

let construct_recursive_input_type () =
  Alcotest.check yojson "json equality"
    ((MyQuery.make
      ~arg:(object
        method otherField = Some "test"
        method enum = None
        method inner = Some (object
          method otherField = Some "inner"
          method enum = Some `SECOND
          method inner = None
        end)
      end)
      ())#variables)
    (Yojson.Basic.from_string {| {
      "arg": {
        "otherField": "test",
        "inner": {
          "otherField": "inner",
          "inner": null,
          "enum": "SECOND"
        },
        "enum": null
      }
    } |})

let tests = [
  "Constructing a recursive input type", `Quick, construct_recursive_input_type;
]
