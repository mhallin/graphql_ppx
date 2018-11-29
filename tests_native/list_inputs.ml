open Test_shared

module MyQuery = [%graphql {|
  query ($arg: ListsInput!) {
    listsInput(arg: $arg)
  }
|}]

type qt = < listsInput : string >

let my_query = (
  module struct
    type t = qt

    let pp formatter (obj: qt) =
      Format.fprintf
        formatter
        "< listsInput = @[%s@] >"
        obj#listsInput

    let equal (a: qt) (b: qt) =
      a#listsInput = b#listsInput

  end : Alcotest.TESTABLE with type t = qt)

let allows_none_in_lists_of_nullable () =
  test_json
    ((MyQuery.make
      ~arg:(object
        method nullableOfNullable = Some [| Some "x"; None; Some"y" |]
        method nullableOfNonNullable = None
        method nonNullableOfNullable = [|Some "a"; None; Some "b"|]
        method nonNullableOfNonNullable = [|"1"; "2"; "3"|]
      end)
      ())#variables)
    (Yojson.Basic.from_string {| {
      "arg": {
        "nullableOfNullable": ["x", null, "y"],
        "nullableOfNonNullable": null,
        "nonNullableOfNullable": ["a", null, "b"],
        "nonNullableOfNonNullable": ["1", "2", "3"]
      }
    } |})

let tests = [
  "Allows None in lists of nullable types", `Quick, allows_none_in_lists_of_nullable
]
