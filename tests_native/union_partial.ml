module MyQuery = [%graphql {|
  {
    dogOrHuman {
      ...on Dog {
        name
        barkVolume
      }
    }
  }
|}]

type qt = <
  dogOrHuman : [
    | `Dog of < name : string ; barkVolume : float >
    | `Nonexhaustive
  ]
>

let my_query = (
  module struct
    type t = qt

    let pp formatter (obj: qt) =
      Format.fprintf formatter "< dogOrHuman = %a >"
        (fun formatter -> function
           | `Dog dog -> Format.fprintf formatter "`Dog @[<>< name = %a ; barkVolume = %a >@]"
                           Format.pp_print_string dog#name
                           Format.pp_print_float dog#barkVolume
           | `Nonexhaustive -> Format.fprintf formatter "`Nonexhaustive")
        obj#dogOrHuman
    
    let equal (a: qt) (b: qt) = 
      match a#dogOrHuman, b#dogOrHuman with
      | (`Dog a), (`Dog b) -> a#name = b#name && a#barkVolume = b#barkVolume
      | `Nonexhaustive, `Nonexhaustive -> true
      | _ -> false
  end : Alcotest.TESTABLE with type t = qt
)

let decodes_non_exhaustive_query () =
  Alcotest.check my_query "result equality"
    (MyQuery.parse (Yojson.Basic.from_string{| {
      "dogOrHuman": {
        "__typename": "Human",
        "name": "Max"
      }
    } |}))
    (object
      method dogOrHuman = `Nonexhaustive
    end)

let tests = [
  "Decodes non-exhaustive query", `Quick, decodes_non_exhaustive_query;
]
