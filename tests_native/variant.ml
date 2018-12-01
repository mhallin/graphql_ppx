open Test_shared

module MyQuery = [%graphql {|
  mutation {
    mutationWithError @bsVariant {
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

type qt = <
  mutationWithError : [
    | `Value of < stringField : string >
    | `Errors of < field : [ | `FIRST | `SECOND | `THIRD ] ; message : string > array
  ]
>

let my_query = (
  module struct
    type t = qt

    let pp formatter (obj: qt) =
      Format.fprintf formatter "< mutationWithError = %a >"
        (fun formatter -> function
           | `Value v -> Format.fprintf formatter "`Value @[<>< stringField = %a >@]"
                           Format.pp_print_string v#stringField
           | `Errors v -> Format.fprintf formatter "`Errors %a"
                            (print_array (fun formatter v ->
                                 Format.fprintf formatter "< field = %a ; message = %a >"
                                   Format.pp_print_string
                                   (match v#field with | `FIRST -> "FIRST" | `SECOND -> "SECOND" | `THIRD -> "THIRD")
                                   Format.pp_print_string v#message))
                            v)
        obj#mutationWithError

    let equal (a: qt) (b: qt) =
      match a#mutationWithError, b#mutationWithError with
      | (`Value a), (`Value b) -> a#stringField = b#stringField
      | (`Errors a), (`Errors b) ->
        array_zipmap
          (fun a b -> a#field = b#field && a#message = b#message)
          a b |> Array.for_all (fun x -> x)
      | _ -> false

  end : Alcotest.TESTABLE with type t = qt)

let converts_into_variant () =
  Alcotest.check my_query "result equality"
  (MyQuery.parse (Yojson.Basic.from_string {| {
    "mutationWithError": {
      "value": {
        "stringField": "a string"
      }
    }
  } |}))
  (object
    method mutationWithError = `Value (object
      method stringField = "a string"
    end)
  end)

let tests = [
  "Converts object into variant", `Quick, converts_into_variant;
]
