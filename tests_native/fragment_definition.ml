open Test_shared

module Fragments = [%graphql {|
  fragment listFragment on Lists {
    nullableOfNullable
    nullableOfNonNullable
  }
|}]

type ft = < nullableOfNullable : string option array option; nullableOfNonNullable : string array option >

module MyQuery = [%graphql {|
  query {
    l1: lists {
      ...Fragments.ListFragment
    }

    l2: lists {
      ...Fragments.ListFragment @bsField(name: "frag1")
      ...Fragments.ListFragment @bsField(name: "frag2")
    }
  }
|}]

type qt = < l1 : ft ; l2 : < frag1 : ft ; frag2 : ft > >

let print_fragment formatter (obj: ft) =
  Format.fprintf
    formatter
    "< nullableOfNullable = @[%a@]; nullableOfNonNullable = @[%a@] >"
    (Format.pp_print_string |> print_option |> print_array |> print_option) obj#nullableOfNullable
    (Format.pp_print_string |> print_array |> print_option) obj#nullableOfNonNullable

let fragment_equal a b =
  a#nullableOfNullable = b#nullableOfNullable &&
  a#nullableOfNonNullable = b#nullableOfNonNullable

let fragment = (
  module struct
    type t = ft

    let pp = print_fragment

    let equal = fragment_equal

  end : Alcotest.TESTABLE with type t = ft)

let my_query = (
  module struct
    type t = qt

    let pp formatter (obj: qt) =
      Format.fprintf
        formatter
        "< l1 = @[%a@]; l2 = @[<>< frag1 = @[%a@]; frag2 = @[%a@] >@] >"
        print_fragment obj#l1
        print_fragment obj#l2#frag1
        print_fragment obj#l2#frag2

    let equal a b =
      fragment_equal a#l1 b#l1 &&
      fragment_equal a#l2#frag1 b#l2#frag1 &&
      fragment_equal a#l2#frag2 b#l2#frag2

  end : Alcotest.TESTABLE with type t = qt)


let decodes_the_fragment () =
  Alcotest.check my_query "query result equality"
    (MyQuery.parse (Yojson.Basic.from_string({|
      {
        "l1": {"nullableOfNullable": ["a", null, "b"]},
        "l2": {"nullableOfNullable": ["a", null, "b"]}
      }|})))
    (object
      method l1 = object
        method nullableOfNullable = Some([| Some "a"; None; Some "b" |])
        method nullableOfNonNullable = None
      end

      method l2 = object
        method frag1 = object
          method nullableOfNullable = Some([| Some "a"; None; Some "b" |])
          method nullableOfNonNullable = None
        end
        method frag2 = object
          method nullableOfNullable = Some([| Some "a"; None; Some "b" |])
          method nullableOfNonNullable = None
        end
      end
    end)

let tests = [
  "Decodes the fragment", `Quick, decodes_the_fragment;
]
