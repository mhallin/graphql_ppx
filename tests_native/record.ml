type scalars = {
  string: string;
  int: int;
}

let scalars = (
  module struct
    type t = scalars

    let pp formatter (obj: scalars) =
      Format.fprintf
        formatter
        "{ string = %a ; int = %a }"
        Format.pp_print_string obj.string
        Format.pp_print_int obj.int
    
    let equal (a: scalars) (b: scalars) =
      a.string = b.string && a.int = b.int

  end : Alcotest.TESTABLE with type t = scalars)

type dog = {
  name: string;
  barkVolume: float;
}

let dog = (
  module struct
    type t = dog

    let pp formatter (obj: dog) =
      Format.fprintf
        formatter
        "{ name = %a ; barkVolume = %a }"
        Format.pp_print_string obj.name
        Format.pp_print_float obj.barkVolume

    let equal (a: dog) (b: dog) =
      a.name = b.name && a.barkVolume = b.barkVolume

  end : Alcotest.TESTABLE with type t = dog)

module MyQuery = [%graphql {|
  {
    variousScalars @bsRecord {
      string
      int
    }
  }
|}]

type qt = < variousScalars : scalars >

let my_query = (
  module struct
    type t = qt

    let pp formatter (obj: qt) =
      Format.fprintf
        formatter
        "< variousScalars = @[%a@] >"
        (Alcotest.pp scalars) obj#variousScalars

    let equal (a: qt) (b: qt) =
      Alcotest.equal scalars a#variousScalars b#variousScalars

  end : Alcotest.TESTABLE with type t = qt)

module ExternalFragmentQuery = [%graphql {|
  fragment Fragment on VariousScalars @bsRecord {
    string
    int
  }

  {
    variousScalars {
      ...Fragment
    }
  }
|}]

module InlineFragmentQuery = [%graphql {|
  {
    dogOrHuman {
      ...on Dog @bsRecord {
        name
        barkVolume
      }
    }
  }
|}]

type if_qt = < dogOrHuman : [ | `Dog of dog | `Nonexhaustive ] >

let inline_fragment_query = (
  module struct
    type t = if_qt

    let pp formatter (obj: if_qt) =
      Format.fprintf
        formatter
        "< dogOrHuman = @[%a@] >"
        (fun formatter v -> match v with
        | `Dog d -> Format.fprintf
          formatter "`Dog %a" (Alcotest.pp dog) d
        | `Nonexhaustive -> Format.fprintf formatter "`Nonexhaustive")
        obj#dogOrHuman

    let equal (a: if_qt) (b: if_qt) =
      match a#dogOrHuman, b#dogOrHuman with
      | (`Dog a), (`Dog b) -> Alcotest.equal dog a b
      | `Nonexhaustive, `Nonexhaustive -> true
      | _ -> false

  end : Alcotest.TESTABLE with type t = if_qt)

module UnionExternalFragmentQuery = [%graphql {|
  fragment DogFragment on Dog @bsRecord {
    name
    barkVolume
  }

  {
    dogOrHuman {
      ...on Dog {
        ...DogFragment
      }
    }
  }
|}]

let decodes_record_in_selection () =
  Alcotest.check my_query "query result equality"
    (MyQuery.parse (Yojson.Basic.from_string {| {"variousScalars": {"string": "a string", "int": 123}} |}))
    (object
      method variousScalars = { string = "a string"; int = 123 }
    end)

let decodes_record_in_external_fragment () =
  Alcotest.check my_query "query result equality"
    (ExternalFragmentQuery.parse (Yojson.Basic.from_string {| {"variousScalars": {"string": "a string", "int": 123}} |}))
    (object
      method variousScalars = { string = "a string"; int = 123 }
    end)

let decodes_record_in_inline_fragment () =
  Alcotest.check inline_fragment_query "query result equality"
    (InlineFragmentQuery.parse (Yojson.Basic.from_string {| {"dogOrHuman": {"__typename": "Dog", "name": "name", "barkVolume": 123}} |}))
    (object
      method dogOrHuman = `Dog { name = "name"; barkVolume = 123.0 }
    end)

let decodes_record_in_external_fragment_on_union_selections () =
  Alcotest.check inline_fragment_query "query result equality"
    (UnionExternalFragmentQuery.parse (Yojson.Basic.from_string {| {"dogOrHuman": {"__typename": "Dog", "name": "name", "barkVolume": 123}} |}))
    (object
      method dogOrHuman = `Dog { name = "name"; barkVolume = 123.0 }
    end)

let tests = [
  "Decodes a record in a selection", `Quick, decodes_record_in_selection;
  "Decodes a record in an external fragment", `Quick, decodes_record_in_external_fragment;
  "Decodes a record in an inline fragment", `Quick, decodes_record_in_inline_fragment;
  "Decodes a record in an external fragment on union selections", `Quick, decodes_record_in_external_fragment_on_union_selections;
]
