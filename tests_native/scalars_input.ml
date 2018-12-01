open Test_shared

module MyQuery = [%graphql {|
  query ($arg: VariousScalarsInput!) {
    scalarsInput(arg: $arg)
  }
|}]

let includes_non_nulled_arguments () =
  Alcotest.check yojson "json equality"
  ((MyQuery.make
    ~arg:(object
      method nullableString = Some "a nullable string"
      method string = "a string"
      method nullableInt = Some 456
      method int = 123
      method nullableFloat = Some 567.5
      method float = 1234.5
      method nullableBoolean = Some false
      method boolean = true
      method nullableID = Some "a nullable ID"
      method id = "an ID"
    end)
    ())#variables)
    (Yojson.Basic.from_string {| {
      "arg": {
        "nullableString": "a nullable string",
        "string": "a string",
        "nullableInt": 456,
        "int": 123,
        "nullableFloat": 567.5,
        "float": 1234.5,
        "nullableBoolean": false,
        "boolean": true,
        "nullableID": "a nullable ID",
        "id": "an ID"
      }
    } |})

let tests = [
  "Includes non-nulled arguments", `Quick, includes_non_nulled_arguments;
]
