open Ppxlib

open Ast_helper

let generate_modules _ _ =
  Mod.mk (Pmod_structure [])