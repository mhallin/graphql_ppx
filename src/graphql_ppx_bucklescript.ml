module Bucklescript_ppx = Graphql_ppx_base.Mapper(Output_bucklescript_module)

let () =
  Migrate_parsetree.Compiler_libs.Ast_mapper.register
  "graphql" (fun argv -> Bucklescript_ppx.mapper argv)
