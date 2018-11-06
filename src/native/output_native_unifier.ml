open Graphql_ppx_base
open Graphql_ast
open Source_pos
open Generator_utils

open Ppxlib
open Parsetree
open Asttypes

open Type_utils
open Output_native_utils

exception Unimplemented of string


let make_make_fun config variable_defs =
  let make_make_triple loc variables =
    [%expr object
      method query = ppx_printed_query
      method variables = [%e variables]
      method parse = parse
    end] in
  let loc = Location.none in
  match variable_defs with
  | Some { item; span } -> begin
      let rec make_labelled_function defs body = match defs with
        | [] -> [%expr fun () -> [%e body]] [@metaloc config.map_loc span |> conv_loc]
        | (name, def) :: tl -> let name_loc = config.map_loc name.span |> conv_loc in
          Ast_helper.(
            Exp.fun_ 
              ~loc:name_loc
              (match def.vd_type.item with
               | Tr_non_null_list _ | Tr_non_null_named _ -> Labelled name.item
               | Tr_list _ | Tr_named _ -> Optional name.item)
              None
              (Pat.var ~loc:name_loc {txt=name.item; loc=name_loc})
              (make_labelled_function tl body))
      in
      let make_object_function defs body =
        let rec generate_bindings defs = match defs with
          | [] -> body
          | (name, _) :: tl -> let name_loc = config.map_loc name.span |> conv_loc in
            Ast_helper.Exp.let_ ~loc:name_loc Nonrecursive [
              Ast_helper.(Vb.mk
                            ~loc:name_loc
                            (Pat.var ~loc:name_loc {txt=name.item; loc=name_loc})
                            (Exp.send [%expr variables] {txt=name.item; loc=name_loc}))
            ] (generate_bindings tl)
        in
        [%expr fun variables -> [%e generate_bindings defs]]
      in
      let make_var_ctor defs =
        defs
        |> List.map (fun (name, def) -> 
            let parser_ = (
              Output_native_encoder.parser_for_type config.schema (config.map_loc name.span) (
                to_native_type_ref (to_schema_type_ref def.vd_type.item)
              )
            ) in
            let loc = config.map_loc name.span |> conv_loc in
            [%expr
              (
                [%e Ast_helper.Exp.constant ~loc (Pconst_string (name.item, None))],
                [%e parser_] [%e Ast_helper.Exp.ident ~loc {txt=Longident.parse name.item; loc}]
              )] [@metaloc loc]
          )
        |> Ast_helper.Exp.array in 
      let loc = config.map_loc span |> conv_loc in
      let variable_ctor_body = 
        [%expr `Assoc ([%e make_var_ctor item] |> Array.to_list)] [@metaloc loc]
      in
      (
        make_labelled_function item (make_make_triple loc variable_ctor_body),
        make_object_function item (make_make_triple loc variable_ctor_body)
      )
    end
  | None -> begin
      (
        [%expr fun () -> [%e make_make_triple Location.none [%expr `Null]]],
        [%expr fun (_: < >) -> [%e make_make_triple Location.none [%expr `Null]]]
      )
    end
