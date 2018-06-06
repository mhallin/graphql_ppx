module VisitorImpl = struct
  open Traversal_utils
  open Source_pos
  open Graphql_ast
  open Schema

  type arg_pos =
    | Directive of string
    | Field of string * string

  type known_args = (arg_pos * argument_meta list) option

  type t = known_args ref
  include Traversal_utils.AbstractVisitor

  let report_error ctx span pos arg_name =
    let msg = match pos with
      | Directive dir_name -> Printf.sprintf "Unknown argument \"%s\" on directive \"%s\"" arg_name dir_name
      | Field (field_name, type_name)
        -> Printf.sprintf "Unknown argument \"%s\" on field \"%s\" of type \"%s\"" arg_name field_name type_name
    in
    Context.push_error ctx span msg

  let enter_argument known_args ctx (name, _) =
    match !known_args with
    | None -> ()
    | Some (pos, known_args) ->
      if not @@ List.exists (fun am -> am.am_name = name.item) known_args then
        report_error ctx name.span pos name.item


  let enter_directive known_args ctx directive =
    known_args := Schema.lookup_directive ctx.schema directive.item.d_name.item
                  |> Option.map (fun dm -> (Directive dm.dm_name, dm.dm_arguments))

  let exit_directive known_args _ _ =
    known_args := None

  let enter_field known_args ctx field =
    let field_name = field.item.fd_name.item in
    known_args := match Context.parent_type ctx with
      | Some parent_type -> Schema.lookup_field parent_type field_name
                            |> Option.map (fun f ->
                                (Field (field_name, Schema.type_name parent_type), f.fm_arguments))
      | None -> None

  let exit_field known_args _ _ =
    known_args := None

end

module Visitor = Traversal_utils.Visitor(VisitorImpl)

let visit_document ctx doc =
  Visitor.visit_document (ref None) ctx doc
