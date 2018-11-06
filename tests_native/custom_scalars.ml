open Test_shared

module My_query = [%graphql {|
  query ($opt: CustomScalar, $req: CustomScalar!) {
    customScalarField(argOptional: $opt, argRequired: $req) {
      nullable
      nonNullable
    }
  }
|}]

type qt = < customScalarField : < nullable : Yojson.Basic.json option ; nonNullable : Yojson.Basic.json > >

let my_query = (
  module struct
    type t = qt

    let pp formatter (obj: qt) =
      Format.fprintf
        formatter
        "< customScalarField = @[<><nullable = @[%a@]; nonNullable = @[%a@] >@] >"
        (Yojson.Basic.pretty_print ~std:false |> print_option) obj#customScalarField#nullable
        (Yojson.Basic.pretty_print ~std:false) obj#customScalarField#nonNullable

    let equal a b =
      a#customScalarField#nullable = b#customScalarField#nullable &&
      a#customScalarField#nonNullable = b#customScalarField#nonNullable

  end : Alcotest.TESTABLE with type t = qt)

let encodes_json_objects () =
  test_json
    (My_query.make ~opt:(`Int 123) ~req:(`Int 456) ())#variables
    (Yojson.Basic.from_string {| { "opt": 123, "req": 456 } |})

let encodes_json_objects_from_obj () =
  test_json
    (My_query.makeWithVariables
      (object
        method opt = Some (`Int 123)
        method req = `Int 456
      end))#variables
    (Yojson.Basic.from_string {| { "opt": 123, "req": 456 } |})

let decodes_to_json () =
  Alcotest.check my_query "query equality"
    (My_query.parse (Yojson.Basic.from_string {|{"customScalarField": { "nullable": 123, "nonNullable": 456 }}|}))
    (object
      method customScalarField = object
        method nullable = Some (`Int 123)
        method nonNullable = `Int 456
      end
    end)

let tests = [
  "Encodes custom scalar variables as Json objects", `Quick, encodes_json_objects;
  "Encodes nullable scalar variables as optional Json objects", `Quick, encodes_json_objects_from_obj;
  "Decodes results to JSON", `Quick, decodes_to_json;
]
