module Visitor: Traversal_utils.VisitorSig = struct
  open Traversal_utils
  open Source_pos
  open Graphql_ast

  include AbstractVisitor

  type t = (string, string spanning) Hashtbl.t

  let make_self () = Hashtbl.create 0

  let enter_operation_definition self _ def =
    let () = Hashtbl.clear self in
    match def.item.o_variable_definitions with
    | None -> ()
    | Some { item; _ } ->
      List.iter (fun (name, _) -> Hashtbl.add self name.item name) item

  let enter_variable_value self _ name =
    Hashtbl.remove self name.item

  let exit_operation_definition self ctx def =
    Hashtbl.iter
      (fun _ v -> 
         let message = match def.item.o_name with
           | None -> Printf.sprintf "Variable \"$%s\" is never used." v.item
           | Some name -> Printf.sprintf "Variable \"$%s\" is never used in operation \"%s\"" v.item name.item
         in
         Context.push_error ctx v.span message)
      self

end
