open Graphql_ppx_base
open Graphql_ast
open Source_pos
open Generator_utils

open Ast_402
open Parsetree
open Asttypes

open Type_utils
open Output_bucklescript_utils

exception Unimplemented of string


let make_make_fun config variable_defs =
  let make_make_triple loc variables =
    Ast_helper.Exp.extension ~loc:loc
      ({txt = "bs.obj"; loc = loc},
       PStr [
         [%stri { 
           query = ppx_printed_query; 
           variables = [%e variables]; 
           parse = parse; }] [@metaloc loc]
       ]) in
  match variable_defs with
  | Some { item; span } -> begin
      let rec make_labelled_function defs body = match defs with
        | [] -> [%expr fun () -> [%e body]] [@metaloc config.map_loc span |> conv_loc]
        | (name, def) :: tl -> let name_loc = config.map_loc name.span |> conv_loc in
          Ast_helper.(
            Exp.fun_ 
              ~loc:name_loc
              (match def.vd_type.item with
               | Tr_non_null_list _ | Tr_non_null_named _ -> name.item
               | Tr_list _ | Tr_named _ -> "?" ^ name.item)
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
                            [%expr variables##[%e Exp.ident {txt=Longident.Lident name.item; loc=name_loc}]])
            ] (generate_bindings tl)
        in
        [%expr fun variables -> [%e generate_bindings defs]]
      in
      let make_var_ctor defs =
        defs
        |> List.map (fun (name, def) -> 
            let parser_ = (
              Output_bucklescript_encoder.parser_for_type config.schema (config.map_loc name.span) (
                to_native_type_ref (to_schema_type_ref def.vd_type.item)
              )
            ) in
            let loc = config.map_loc name.span |> conv_loc in
            [%expr
              (
                [%e Ast_helper.Exp.constant ~loc (Const_string (name.item, None))],
                [%e parser_] [%e Ast_helper.Exp.ident ~loc {txt=Longident.parse name.item; loc}]
              )] [@metaloc loc]
          )
          |> Ast_helper.Exp.array in 
      let loc = config.map_loc span |> conv_loc in
      let variable_ctor_body = 
        [%expr Js.Json.object_ (
          [%e make_var_ctor item]
          |> [%e Output_bucklescript_encoder.filter_out_null_values]
          |> Js.Dict.fromArray
          )] [@metaloc loc]
      in
      (
        make_labelled_function item (make_make_triple loc variable_ctor_body),
        make_object_function item (make_make_triple loc variable_ctor_body)
      )
    end
  | None -> begin
      (
        [%expr fun () -> [%e make_make_triple Location.none [%expr Js.Json.null]]],
        [%expr fun (_: < > Js.t) -> [%e make_make_triple Location.none [%expr Js.Json.null]]]
      )
    end
