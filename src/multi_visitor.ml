module NullVisitor: Traversal_utils.VisitorSig = struct
  type t = ()
  include Traversal_utils.AbstractVisitor
  let make_self _ = ()
end

module Visitor(H: Traversal_utils.VisitorSig)(T: Traversal_utils.VisitorSig): Traversal_utils.VisitorSig = struct
  type t = H.t * T.t

  let make_self () = (H.make_self (), T.make_self ())

  let enter_document (h, t) ctx doc =
    let () = H.enter_document h ctx doc in
    T.enter_document t ctx doc
  let exit_document (h, t) ctx doc =
    let () = H.exit_document h ctx doc in
    T.exit_document t ctx doc

  let enter_operation_definition (h, t) ctx def =
    let () = H.enter_operation_definition h ctx def in
    T.enter_operation_definition t ctx def
  let exit_operation_definition (h, t) ctx def =
    let () = H.exit_operation_definition h ctx def in
    T.exit_operation_definition t ctx def

  let enter_fragment_definition (h, t) ctx def =
    let () = H.enter_fragment_definition h ctx def in
    T.enter_fragment_definition t ctx def
  let exit_fragment_definition (h, t) ctx def =
    let () = H.exit_fragment_definition h ctx def in
    T.exit_fragment_definition t ctx def

  let enter_variable_definition (h, t) ctx def =
    let () = H.enter_variable_definition h ctx def in
    T.enter_variable_definition t ctx def
  let exit_variable_definition (h, t) ctx def =
    let () = H.exit_variable_definition h ctx def in
    T.exit_variable_definition t ctx def

  let enter_directive (h, t) ctx d =
    let () = H.enter_directive h ctx d in
    T.enter_directive t ctx d
  let exit_directive (h, t) ctx d =
    let () = H.exit_directive h ctx d in
    T.exit_directive t ctx d

  let enter_argument (h, t) ctx a =
    let () = H.enter_argument h ctx a in
    T.enter_argument t ctx a
  let exit_argument (h, t) ctx a =
    let () = H.exit_argument h ctx a in
    T.exit_argument t ctx a

  let enter_selection_set (h, t) ctx s =
    let () = H.enter_selection_set h ctx s in
    T.enter_selection_set t ctx s
  let exit_selection_set (h, t) ctx s =
    let () = H.exit_selection_set h ctx s in
    T.exit_selection_set t ctx s

  let enter_field (h, t) ctx f =
    let () = H.enter_field h ctx f in
    T.enter_field t ctx f
  let exit_field (h, t) ctx f =
    let () = H.exit_field h ctx f in
    T.exit_field t ctx f

  let enter_fragment_spread (h, t) ctx fs =
    let () = H.enter_fragment_spread h ctx fs in
    T.enter_fragment_spread t ctx fs
  let exit_fragment_spread (h, t) ctx fs =
    let () = H.exit_fragment_spread h ctx fs in
    T.exit_fragment_spread t ctx fs

  let enter_inline_fragment (h, t) ctx if_ =
    let () = H.enter_inline_fragment h ctx if_ in
    T.enter_inline_fragment t ctx if_
  let exit_inline_fragment (h, t) ctx if_ =
    let () = H.exit_inline_fragment h ctx if_ in
    T.exit_inline_fragment t ctx if_

  let enter_null_value (h, t) ctx v =
    let () = H.enter_null_value h ctx v in
    T.enter_null_value t ctx v
  let exit_null_value (h, t) ctx v =
    let () = H.exit_null_value h ctx v in
    T.exit_null_value t ctx v

  let enter_int_value (h, t) ctx v =
    let () = H.enter_int_value h ctx v in
    T.enter_int_value t ctx v
  let exit_int_value (h, t) ctx v =
    let () = H.exit_int_value h ctx v in
    T.exit_int_value t ctx v

  let enter_float_value (h, t) ctx v =
    let () = H.enter_float_value h ctx v in
    T.enter_float_value t ctx v
  let exit_float_value (h, t) ctx v =
    let () = H.exit_float_value h ctx v in
    T.exit_float_value t ctx v

  let enter_string_value (h, t) ctx v =
    let () = H.enter_string_value h ctx v in
    T.enter_string_value t ctx v
  let exit_string_value (h, t) ctx v =
    let () = H.exit_string_value h ctx v in
    T.exit_string_value t ctx v

  let enter_bool_value (h, t) ctx v =
    let () = H.enter_bool_value h ctx v in
    T.enter_bool_value t ctx v
  let exit_bool_value (h, t) ctx v =
    let () = H.exit_bool_value h ctx v in
    T.exit_bool_value t ctx v

  let enter_enum_value (h, t) ctx v =
    let () = H.enter_enum_value h ctx v in
    T.enter_enum_value t ctx v
  let exit_enum_value (h, t) ctx v =
    let () = H.exit_enum_value h ctx v in
    T.exit_enum_value t ctx v

  let enter_variable_value (h, t) ctx v =
    let () = H.enter_variable_value h ctx v in
    T.enter_variable_value t ctx v
  let exit_variable_value (h, t) ctx v =
    let () = H.exit_variable_value h ctx v in
    T.exit_variable_value t ctx v

  let enter_list_value (h, t) ctx v =
    let () = H.enter_list_value h ctx v in
    T.enter_list_value t ctx v
  let exit_list_value (h, t) ctx v =
    let () = H.exit_list_value h ctx v in
    T.exit_list_value t ctx v

  let enter_object_value (h, t) ctx v =
    let () = H.enter_object_value h ctx v in
    T.enter_object_value t ctx v
  let exit_object_value (h, t) ctx v =
    let () = H.exit_object_value h ctx v in
    T.exit_object_value t ctx v

  let enter_object_field (h, t) ctx f =
    let () = H.enter_object_field h ctx f in
    T.enter_object_field t ctx f
  let exit_object_field (h, t) ctx f =
    let () = H.exit_object_field h ctx f in
    T.exit_object_field t ctx f

end
