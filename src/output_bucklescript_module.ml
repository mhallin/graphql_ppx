open Result_structure
open Schema
open Generator_utils

open Ast_402
open Asttypes
open Parsetree
open Ast_helper

module StringSet = Set.Make(String)
module VariableFinderImpl = struct
  type t = StringSet.t ref
  let make_self _ = ref StringSet.empty

  include Traversal_utils.AbstractVisitor

  let enter_variable_value self _ v =
    self := StringSet.add v.Source_pos.item !self

  let from_self (self: t) : StringSet.t = !self
end

module VariableFinder = Traversal_utils.Visitor(VariableFinderImpl)

let find_variables config document =
  let ctx = Traversal_utils.make_context config document in
  VariableFinderImpl.from_self (VariableFinder.visit_document ctx document)

let ret_type_magic = [
  (* Some functor magic to determine the return type of parse *)
  [%stri module type mt_ret = sig type t end];
  [%stri type 'a typed_ret = (module mt_ret with type t = 'a)];
  [%stri let ret_type (type a) (f: _ -> a) = (let module MT_Ret = struct type t = a end in (module MT_Ret): a typed_ret)];
  [%stri module MT_Ret = (val ret_type parse)];
  [%stri type t = MT_Ret.t];
]

let make_printed_query config document = 
  let source = Graphql_printer.print_document config.schema document in
  let reprinted = match config.output_mode with
    | Apollo_AST -> Ast_serializer_apollo.serialize_document source document
    | String -> Exp.constant (Const_string (source, config.delimiter))
  in
  [
    [%stri let ppx_printed_query = [%e reprinted]];
    [%stri let query = ppx_printed_query];
  ]

let generate_default_operation config variable_defs has_error operation res_structure =
  let parse_fn = Output_bucklescript_decoder.generate_decoder config res_structure in
  if has_error then
    [ [%stri let parse = fun value -> [%e parse_fn]] ]
  else
    let (rec_flag, encoders) = 
      Output_bucklescript_encoder.generate_encoders config (Result_structure.res_loc res_structure) variable_defs in
    let make_fn, make_with_variables_fn = Unifier.make_make_fun config variable_defs in
    List.concat [
      make_printed_query config [Graphql_ast.Operation operation];
      List.concat [
        [ [%stri let parse = fun value -> [%e parse_fn]] ];
        (if rec_flag = Recursive then
           [{
             pstr_desc = (Pstr_value (rec_flag, encoders |> Array.to_list));
             pstr_loc = Location.none;
           }]
         else 
           encoders
           |> Array.map (fun encoder -> { pstr_desc = (Pstr_value (Nonrecursive, [encoder])); pstr_loc = Location.none })
           |> Array.to_list
        );
        [
          [%stri let make = [%e make_fn]];
          [%stri let makeWithVariables = [%e make_with_variables_fn]];
        ];
      ];
      ret_type_magic
    ]

let generate_fragment_module config name required_variables has_error fragment res_structure =
  let parse_fn = Output_bucklescript_decoder.generate_decoder config res_structure in
  let variable_names = find_variables config [Graphql_ast.Fragment fragment] |> StringSet.elements in
  let variable_fields = variable_names |> List.map (fun name ->
      (name, [], Ast_helper.Typ.constr { txt = Longident.Lident "unit"; loc = Location.none} [])) in
  let variable_obj_type = (Ast_helper.Typ.constr { txt = Longident.parse "Js.t"; loc = Location.none} [
      (Ast_helper.Typ.object_ variable_fields Open) ]) in
  let contents = if has_error then
      [ [%stri let make = fun (_vars: [%t variable_obj_type]) value -> [%e parse_fn]] ]
    else
      List.concat [
        make_printed_query config [Graphql_ast.Fragment fragment];
        [
          [%stri let parse = fun value -> [%e parse_fn]];
          [%stri let name = [%e Ast_helper.Exp.constant (Const_string (name, None))]];
        ];
        ret_type_magic
      ]
  in
  let m = Pstr_module {
      pmb_name = { txt = Generator_utils.capitalize_ascii name; loc = Location.none };
      pmb_expr = Mod.structure contents;
      pmb_attributes = [];
      pmb_loc = Location.none;
    } in
  [ { pstr_desc = m; pstr_loc = Location.none; } ]

let generate_operation config = function
  | Mod_default_operation (vdefs, has_error, operation, structure) -> generate_default_operation config vdefs has_error operation structure
  | Mod_fragment (name, req_vars, has_error, fragment, structure) -> generate_fragment_module config name req_vars has_error fragment structure

let generate_modules config operations =
  let generated = List.map (generate_operation config) operations in
  Mod.mk (Pmod_structure (List.concat generated))
