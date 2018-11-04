open Graphql_ppx_base
open Ppxlib

module Native_ppx = Graphql_ppx_base.Rewriter()

let argv = Sys.argv |> Array.to_list

let () = 
  Log.is_verbose := match List.find ((=) "-verbose") argv with
    | _ -> true
    | exception Not_found -> false

let output_mode = match List.find ((=) "-ast-out") argv with
  | _ -> Generator_utils.Apollo_AST
  | exception Not_found -> Generator_utils.String

let verbose_error_handling = match List.find ((=) "-o") argv with
  | _ -> false
  | exception Not_found -> begin match Sys.getenv "NODE_ENV" with
      | "production" -> false
      | _ -> true
      | exception Not_found -> true
    end

let here_dir = Sys.getcwd()

let here_scheme_path = match List.find (is_prefixed "-schema=") argv with
  | arg -> drop_prefix "-schema=" arg
  | exception Not_found -> "graphql_schema.json" (* the default path so it won't break backward compatibility *)

let rewrite ~loc ~path:_ expr =
  let open Ast_402 in
  let open Parsetree in
  match From_current.copy_expression expr with
  | { pexp_desc = Pexp_constant (Const_string (query, delim)); _ } ->
    Native_ppx.rewrite_query
      loc
      delim
      query
      output_mode
      here_dir
      here_scheme_path
      verbose_error_handling
  | _ -> raise (Location.Error (
      Location.error ~loc "[%graphql] accepts a string, e.g. [%graphql {| { query |}]"
    ))

let ext = Extension.declare
    "graphql"
    Extension.Context.module_expr
    Ast_pattern.(single_expr_payload __)
    rewrite

let () = Ppxlib.Driver.register_transformation "graphql" 
