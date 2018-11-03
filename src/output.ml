open Ast_402

module type Module_generator = sig
  val generate_modules:
    Generator_utils.output_config ->
    Result_structure.mod_ list ->
    Parsetree.module_expr
end
