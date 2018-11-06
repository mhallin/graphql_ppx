let yojson = (
  module struct
    type t = Yojson.Basic.json

    let pp formatter t =
      Format.pp_print_text formatter (Yojson.Basic.pretty_to_string t)

    let equal = (=)
  end : Alcotest.TESTABLE with type t = Yojson.Basic.json)

let test_json a b = Alcotest.check yojson "JSON equality" a b

let print_option inner formatter = function
  | None -> Format.pp_print_string formatter "None"
  | Some v -> Format.fprintf formatter "Some(@[%a@])" inner v

let print_array inner formatter value =
  let open Format in
  pp_print_string formatter "[ ";
  Array.iteri (fun idx v ->
    if idx > 0 then pp_print_string formatter "; ";
    pp_open_hovbox formatter 1;
    inner formatter v;
    pp_close_box formatter ();
  ) value;
  pp_print_string formatter " ]"

let array_zipmap f a b =
  let min = min (Array.length a) (Array.length b) in
  Array.init min (fun i -> f a.(i) b.(i))
