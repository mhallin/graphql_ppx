open Traversal_utils

module AllRulesImpl = 
  Multi_visitor.Visitor(Rule_known_argument_names.Visitor)(
    Multi_visitor.Visitor(Rule_no_unused_variables.Visitor)(
      Multi_visitor.NullVisitor))

module AllRules = Visitor(AllRulesImpl)

let find_fragments doc =
  let open Graphql_ast in
  let open Source_pos in
  let lookup = Hashtbl.create 1 in
  let () = List.iter (function 
      | Fragment fragment -> Hashtbl.add lookup fragment.item.fg_name.item fragment.item
      | _ -> ()) doc in
  lookup

let run_validators map_loc schema document = 
  let ctx = {
    map_loc = map_loc;
    fragments = find_fragments document;
    schema = schema;
    errors = ref([]);
    type_stack = [];
    type_literal_stack = [];
    input_type_stack = [];
    input_type_literal_stack = [];
    parent_type_stack = [];
  } in
  let () = AllRules.visit_document ctx document in
  match ! (ctx.errors) with
  | [] -> None
  | errs -> Some errs
