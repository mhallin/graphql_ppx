open Graphql_ast
open Source_pos
open Schema

open Parsetree
open Asttypes

open Type_utils

exception Unimplemented of string


let rec make_make_fun map_loc schema document =
  let make_make_triple loc variables =
    Ast_helper.Exp.extension ~loc:loc
      ({txt = "bs.obj"; loc = loc},
       PStr [
         [%stri { 
           query = query; 
           variables = [%e variables]; 
           parse = parse; }] [@metaloc loc]
       ]) in
  match document with
  | [Operation { item = { o_variable_definitions = Some { item; span } }}] -> begin
      let rec make_labelled_function defs body = match defs with
        | [] -> [%expr fun () -> [%e body]] [@metaloc map_loc span]
        | (name, def) :: tl -> let name_loc = map_loc name.span in
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
          | (name, def) :: tl -> let name_loc = map_loc name.span in
            Ast_helper.Exp.let_ ~loc:name_loc Nonrecursive [
              Ast_helper.(Vb.mk
                ~loc:name_loc
                (Pat.var ~loc:name_loc {txt=name.item; loc=name_loc})
                [%expr variables##[%e Exp.ident {txt=Longident.Lident name.item; loc=name_loc}]])
            ] (generate_bindings tl)
        in
        [%expr fun variables -> [%e generate_bindings defs]]
      in
      let rec make_body defs = match defs with
        | (name, def) :: tl -> 
          let parser_ = (
            Variable_encoder.parser_for_type schema (map_loc name.span) (
              to_native_type_ref (to_schema_type_ref def.vd_type.item)
            )
          ) in 
          begin match parser_ with
            | None -> make_body tl
            | Some parser_ ->
              [%expr
                (
                  [%e Ast_helper.Exp.constant ~loc:(map_loc name.span) (Const_string (name.item, None))],
                  [%e parser_] [%e Ast_helper.Exp.ident ~loc:(map_loc name.span) {txt=Longident.parse name.item; loc=map_loc name.span}]
                ) :: [%e make_body tl]] [@metaloc map_loc name.span]
          end
        | [] -> [%expr []] [@metaloc map_loc span]
      in
      let loc = map_loc span in
      let variable_ctor_body = 
        [%expr Js.Json.object_ (Js.Dict.fromList [%e make_body item])] [@metaloc loc]
      in
      (
        make_labelled_function item (make_make_triple loc variable_ctor_body),
        make_object_function item (make_make_triple loc variable_ctor_body)
      )
    end
  | [Operation { item = { o_variable_definitions = None }; span }] -> begin
      let loc = map_loc span in
      (
        [%expr fun () -> [%e make_make_triple loc [%expr Js.Json.null]]],
        [%expr fun (_: < > Js.t) -> [%e make_make_triple loc [%expr Js.Json.null]]]
      )
    end
  | _ -> raise @@ Unimplemented "variables on other than singular queries/mutations"
