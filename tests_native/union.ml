module MyQuery = [%graphql {|
  {
    dogOrHuman {
      ...on Dog {
        name
        barkVolume
      }

      ...on Human {
        name
      }
    }
  }
|}]

type qt = <
  dogOrHuman : [
    | `Dog of < name : string ; barkVolume : float >
    | `Human of < name : string >
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
           | `Human human -> Format.fprintf formatter "`Human @[<>< name = %a >@]"
                               Format.pp_print_string human#name)
        obj#dogOrHuman
    
    let equal (a: qt) (b: qt) = 
      match a#dogOrHuman, b#dogOrHuman with
      | (`Dog a), (`Dog b) -> a#name = b#name && a#barkVolume = b#barkVolume
      | (`Human a), (`Human b) -> a#name = b#name
      | _ -> false
  end : Alcotest.TESTABLE with type t = qt
)

let decodes_exhaustive_query () =
  Alcotest.check my_query "result equality"
    (MyQuery.parse (Yojson.Basic.from_string{| {
      "dogOrHuman": {
        "__typename": "Dog",
        "name": "Fido",
        "barkVolume": 123
      }
    } |}))
    (object
      method dogOrHuman = `Dog (object
        method name = "Fido"
        method barkVolume = 123.0
      end)
    end)

let tests = [
  "Decodes exhaustive query", `Quick, decodes_exhaustive_query;
]
