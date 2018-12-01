open Test_shared

module MyQuery = [%graphql {|
  query ($arg: NonrecursiveInput!) {
    nonrecursiveInput(arg: $arg)
  }
|}]

type qt = < nonrecursiveInput : string >

let my_query = (
  module struct
    type t = qt

    let pp formatter (t: qt) =
      Format.fprintf
        formatter
        "< nonrecursiveInput = %a >"
        Format.pp_print_string t#nonrecursiveInput

    let equal (a: qt) (b: qt) =
      a#nonrecursiveInput = b#nonrecursiveInput

  end : Alcotest.TESTABLE with type t = qt)

let construct_recursive_input_type () =
  test_json
    ((MyQuery.make
      ~arg:(object
        method field = Some "test"
        method enum = Some `SECOND
      end)
      ())#variables)
    (Yojson.Basic.from_string {| {
      "arg": {
        "field": "test",
        "enum": "SECOND"
      }
    } |})

let tests = [
  "Constructing a recursive input type", `Quick, construct_recursive_input_type;
]
