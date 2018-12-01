open Test_shared

module MyQuery = [%graphql {|
  {
    first: nestedObject {
      __typename
      inner {
        __typename
        inner {
          __typename
          field
        }
      }
    }
  }
|}]

type qt = <
  first: <
    __typename : string ;
    inner : <
      __typename : string ;
      inner : <
        __typename : string ;
        field : string ;
      > option ;
    > option ;
  >
>

let my_query = (
  module struct
    type t = qt

    let pp formatter (obj: qt) =
      Format.fprintf
        formatter
        "< first = @[<>< __typename = %a ; inner = %a >@] >"
        Format.pp_print_string obj#first#__typename
        (fun formatter -> function
           | Some v -> Format.fprintf
                         formatter
                         "Some @[<>< __typename = %a ; inner = %a >@]"
                         Format.pp_print_string v#__typename
                         (fun formatter -> function
                            | Some v -> Format.fprintf
                                          formatter
                                          "Some @[<>< __typename = %a ; field = %a >@]"
                                          Format.pp_print_string v#__typename
                                          Format.pp_print_string v#field
                            | None -> Format.fprintf formatter "None")
                         v#inner
           | None -> Format.fprintf formatter "None")
        obj#first#inner

    let equal (a: qt) (b: qt) =
      a#first#__typename = b#first#__typename &&
      opt_eq
        (fun a b -> 
           a#__typename = b#__typename &&
           opt_eq
             (fun a b ->
                a#__typename = b#__typename && 
                a#field = b#field)
             a#inner b#inner)
        a#first#inner b#first#inner

  end : Alcotest.TESTABLE with type t = qt)

let decodes_typename () =
  Alcotest.check my_query "result equality"
    (MyQuery.parse (Yojson.Basic.from_string {|
      {"first": {"__typename": "NestedObject", "inner": null}}
    |}))
    (object
      method first = (object
        method __typename = "NestedObject"
        method inner = None
      end)
    end)

let tests = [
  "Decodes typename as a non-nullable string", `Quick, decodes_typename;
]
