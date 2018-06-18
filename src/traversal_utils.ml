open Graphql_ast
open Source_pos

type ctx = {
  map_loc: source_position * source_position -> Result_structure.loc;
  fragments: (string, Graphql_ast.fragment) Hashtbl.t;

  schema: Schema.schema;
  errors: (Result_structure.loc * string) list ref;

  type_stack: Schema.type_meta option list;
  type_literal_stack: Schema.type_ref option list;

  input_type_stack: Schema.type_meta option list;
  input_type_literal_stack: Schema.type_ref option list;

  parent_type_stack: Schema.type_meta option list;
}

module type VisitorSig = sig
  type t

  val make_self: unit -> t

  val enter_document: t -> ctx -> document -> unit
  val exit_document: t -> ctx -> document -> unit

  val enter_operation_definition: t -> ctx -> operation spanning -> unit
  val exit_operation_definition: t -> ctx -> operation spanning -> unit

  val enter_fragment_definition: t -> ctx -> fragment spanning -> unit
  val exit_fragment_definition: t -> ctx -> fragment spanning -> unit

  val enter_variable_definition: t -> ctx -> (string spanning * variable_definition) -> unit
  val exit_variable_definition: t -> ctx -> (string spanning * variable_definition) -> unit

  val enter_directive: t -> ctx -> directive spanning -> unit
  val exit_directive: t -> ctx -> directive spanning -> unit

  val enter_argument: t -> ctx -> (string spanning * input_value spanning) -> unit
  val exit_argument: t -> ctx -> (string spanning * input_value spanning) -> unit

  val enter_selection_set: t -> ctx -> selection list spanning -> unit
  val exit_selection_set: t -> ctx -> selection list spanning -> unit

  val enter_field: t -> ctx -> field spanning -> unit
  val exit_field: t -> ctx -> field spanning -> unit

  val enter_fragment_spread: t -> ctx -> fragment_spread spanning -> unit
  val exit_fragment_spread: t -> ctx -> fragment_spread spanning -> unit

  val enter_inline_fragment: t -> ctx -> inline_fragment spanning -> unit
  val exit_inline_fragment: t -> ctx -> inline_fragment spanning -> unit

  val enter_null_value: t -> ctx -> unit spanning -> unit
  val exit_null_value: t -> ctx -> unit spanning -> unit

  val enter_int_value: t -> ctx -> int spanning -> unit
  val exit_int_value: t -> ctx -> int spanning -> unit

  val enter_float_value: t -> ctx -> float spanning -> unit
  val exit_float_value: t -> ctx -> float spanning -> unit

  val enter_string_value: t -> ctx -> string spanning -> unit
  val exit_string_value: t -> ctx -> string spanning -> unit

  val enter_bool_value: t -> ctx -> bool spanning -> unit
  val exit_bool_value: t -> ctx -> bool spanning -> unit

  val enter_enum_value: t -> ctx -> string spanning -> unit
  val exit_enum_value: t -> ctx -> string spanning -> unit

  val enter_variable_value: t -> ctx -> string spanning -> unit
  val exit_variable_value: t -> ctx -> string spanning -> unit

  val enter_list_value: t -> ctx -> input_value spanning list -> unit
  val exit_list_value: t -> ctx -> input_value spanning list -> unit

  val enter_object_value: t -> ctx -> (string spanning * input_value spanning) list -> unit
  val exit_object_value: t -> ctx -> (string spanning * input_value spanning) list -> unit

  val enter_object_field: t -> ctx -> string spanning * input_value spanning -> unit
  val exit_object_field: t -> ctx -> string spanning * input_value spanning -> unit
end

module AbstractVisitor = struct
  let enter_document _self _ctx _ = ()
  let exit_document _self _ctx _ = ()

  let enter_operation_definition _self _ctx _ = ()
  let exit_operation_definition _self _ctx _ = ()

  let enter_fragment_definition _self _ctx _ = ()
  let exit_fragment_definition _self _ctx _ = ()

  let enter_variable_definition _self _ctx _ = ()
  let exit_variable_definition _self _ctx _ = ()

  let enter_directive _self _ctx _ = ()
  let exit_directive _self _ctx _ = ()

  let enter_argument _self _ctx _ = ()
  let exit_argument _self _ctx _ = ()

  let enter_selection_set _self _ctx _ = ()
  let exit_selection_set _self _ctx _ = ()

  let enter_field _self _ctx _ = ()
  let exit_field _self _ctx _ = ()

  let enter_fragment_spread _self _ctx _ = ()
  let exit_fragment_spread _self _ctx _ = ()

  let enter_inline_fragment _self _ctx _ = ()
  let exit_inline_fragment _self _ctx _ = ()

  let enter_null_value _self _ctx _ = ()
  let exit_null_value _self _ctx _ = ()

  let enter_int_value _self _ctx _ = ()
  let exit_int_value _self _ctx _ = ()

  let enter_float_value _self _ctx _ = ()
  let exit_float_value _self _ctx _ = ()

  let enter_string_value _self _ctx _ = ()
  let exit_string_value _self _ctx _ = ()

  let enter_bool_value _self _ctx _ = ()
  let exit_bool_value _self _ctx _ = ()

  let enter_enum_value _self _ctx _ = ()
  let exit_enum_value _self _ctx _ = ()

  let enter_variable_value _self _ctx _ = ()
  let exit_variable_value _self _ctx _ = ()

  let enter_list_value _self _ctx _ = ()
  let exit_list_value _self _ctx _ = ()

  let enter_object_value _self _ctx _ = ()
  let exit_object_value _self _ctx _ = ()

  let enter_object_field _self _ctx _ = ()
  let exit_object_field _self _ctx _ = ()

end

module Context = struct
  let push_type ctx type_ref = 
    {
      ctx with
      type_stack = Option.flat_map (fun type_ref -> Schema.lookup_type ctx.schema (Schema.innermost_name type_ref)) type_ref :: ctx.type_stack;
      type_literal_stack = type_ref :: ctx.type_literal_stack;
    }
  let push_input_type ctx type_ref =
    {
      ctx with
      input_type_stack = Option.flat_map (fun type_ref -> Schema.lookup_type ctx.schema (Schema.innermost_name type_ref)) type_ref ::ctx.input_type_stack;
      input_type_literal_stack = type_ref :: ctx.input_type_literal_stack;
    }
  let push_parent_type ctx =
    let top = match ctx.type_stack with
      | t :: _ -> t
      | [] -> None in
    {
      ctx with
      parent_type_stack = top :: ctx.parent_type_stack;
    }
  let parent_type ctx = match ctx.parent_type_stack with
    | t :: _ -> t
    | [] -> None
  let current_input_type_literal ctx = match ctx.input_type_literal_stack with
    | t :: _ -> t
    | [] -> None
  let push_error ctx loc msg =
    ctx.errors := (ctx.map_loc loc, msg) :: ! (ctx.errors)
end

let rec as_schema_type_ref = function
  | Tr_named { item } -> Schema.Named item
  | Tr_list { item } -> Schema.List (as_schema_type_ref item)
  | Tr_non_null_named { item } -> Schema.NonNull (Schema.Named item)
  | Tr_non_null_list { item } -> Schema.NonNull (Schema.List (as_schema_type_ref item))

module Visitor(V: VisitorSig) = struct
  let enter_input_value self ctx value = match value.item with
    | Iv_null -> V.enter_null_value self ctx (Source_pos.replace value ())
    | Iv_int i -> V.enter_int_value self ctx (Source_pos.replace value i)
    | Iv_float f -> V.enter_float_value self ctx (Source_pos.replace value f)
    | Iv_string s -> V.enter_string_value self ctx (Source_pos.replace value s)
    | Iv_boolean b -> V.enter_bool_value self ctx (Source_pos.replace value b)
    | Iv_enum e -> V.enter_enum_value self ctx (Source_pos.replace value e)
    | Iv_variable v -> V.enter_variable_value self ctx (Source_pos.replace value v)
    | Iv_list l -> V.enter_list_value self ctx l
    | Iv_object o -> V.enter_object_value self ctx o

  let exit_input_value self ctx value = match value.item with
    | Iv_null -> V.exit_null_value self ctx (Source_pos.replace value ())
    | Iv_int i -> V.exit_int_value self ctx (Source_pos.replace value i)
    | Iv_float f -> V.exit_float_value self ctx (Source_pos.replace value f)
    | Iv_string s -> V.exit_string_value self ctx (Source_pos.replace value s)
    | Iv_boolean b -> V.exit_bool_value self ctx (Source_pos.replace value b)
    | Iv_enum e -> V.exit_enum_value self ctx (Source_pos.replace value e)
    | Iv_variable v -> V.exit_variable_value self ctx (Source_pos.replace value v)
    | Iv_list l -> V.exit_list_value self ctx l
    | Iv_object o -> V.exit_object_value self ctx o

  let rec visit_input_value self ctx value =
    let () = enter_input_value self ctx value in
    let () = match value.item with
      | Iv_object fields ->
        List.iter (fun (key, value) ->
            let inner_type =
              Context.current_input_type_literal ctx
              |> Option.flat_map (function 
                  | Schema.Named name | Schema.NonNull (Schema.Named name)
                    -> Schema.lookup_type ctx.schema name
                  | _ -> None)
              |> Option.flat_map (fun t -> Schema.lookup_input_field t key.item)
              |> Option.map (fun am -> am.Schema.am_arg_type) in
            let ctx = Context.push_input_type ctx inner_type in
            let () = V.enter_object_field self ctx (key, value) in
            let () = visit_input_value self ctx value in
            V.exit_object_field self ctx (key, value)
          )
          fields
      | Iv_list items ->
        let inner_type =
          Context.current_input_type_literal ctx
          |> Option.flat_map (function
              | Schema.List inner | Schema.NonNull (Schema.List inner)
                -> Some inner
              | _ -> None) in
        let ctx = Context.push_input_type ctx inner_type in
        List.iter (visit_input_value self ctx) items
      | _ -> () in
    exit_input_value self ctx value

  let rec visit_inline_fragment self ctx inline_fragment =
    let ctx = match inline_fragment.item.if_type_condition with
      | None -> ctx
      | Some { item } -> Context.push_type ctx (Some (Schema.NonNull(Schema.Named(item))))
    in
    let () = V.enter_inline_fragment self ctx inline_fragment in
    let () = visit_directives self ctx inline_fragment.item.if_directives in
    let () = visit_selection_set self ctx inline_fragment.item.if_selection_set in
    V.exit_inline_fragment self ctx inline_fragment

  and visit_fragment_spread self ctx fragment_spread =
    let () = V.enter_fragment_spread self ctx fragment_spread in
    let () = visit_directives self ctx fragment_spread.item.fs_directives in
    V.exit_fragment_spread self ctx fragment_spread

  and visit_field self ctx field =
    let meta_field = Context.parent_type ctx
                     |> Option.flat_map (fun t -> Schema.lookup_field t field.item.fd_name.item) in
    let field_type = Option.map (fun fm -> fm.Schema.fm_field_type) meta_field in
    let field_args = Option.map (fun fm -> fm.Schema.fm_arguments) meta_field in
    let ctx = Context.push_type ctx field_type in
    let () = V.enter_field self ctx field in
    let () = visit_arguments self ctx field_args field.item.fd_arguments in
    let () = visit_directives self ctx field.item.fd_directives in
    let () = match field.item.fd_selection_set with
      | None -> ()
      | Some selection_set -> visit_selection_set self ctx selection_set in
    V.exit_field self ctx field

  and visit_selection self ctx = function
    | Field field -> visit_field self ctx field
    | FragmentSpread fragment_spread ->
      visit_fragment_spread self ctx fragment_spread
    | InlineFragment inline_fragment ->
      visit_inline_fragment self ctx inline_fragment

  and visit_selection_set self ctx selection_set =
    let ctx = Context.push_parent_type ctx in
    let () = V.enter_selection_set self ctx selection_set in
    let () = List.iter (visit_selection self ctx) selection_set.item in
    V.exit_selection_set self ctx selection_set

  and visit_arguments self ctx meta_args = function
    | None -> ()
    | Some { item } -> 
      List.iter (fun (name, value) ->
          let arg_type = meta_args
                         |> Option.flat_map (fun meta_args ->
                             match List.find (fun am -> am.Schema.am_name = name.item) meta_args with
                             | am -> Some(am)
                             | exception Not_found -> None)
                         |> Option.map (fun am -> am.Schema.am_arg_type) in
          let ctx = Context.push_input_type ctx arg_type in
          let () = V.enter_argument self ctx (name, value) in
          let () = visit_input_value self ctx value in
          V.exit_argument self ctx (name, value))
        item

  and visit_directives self ctx =
    List.iter (fun directive ->
        let meta_args = Schema.lookup_directive ctx.schema directive.item.d_name.item
                        |> Option.map (fun d -> d.Schema.dm_arguments) in
        let () = V.enter_directive self ctx directive in
        let () = visit_arguments self ctx meta_args directive.item.d_arguments in
        V.exit_directive self ctx directive)

  let visit_variable_definitions self ctx = function
    | None -> ()
    | Some { item } -> List.iter (fun (name, def) -> 
        let ctx = Context.push_input_type ctx (Some (as_schema_type_ref def.vd_type.item)) in
        let () = V.enter_variable_definition self ctx (name, def) in
        let () = match def.vd_default_value with
          | None -> ()
          | Some value -> visit_input_value self ctx value in
        V.exit_variable_definition self ctx (name, def)
      ) item

  let visit_operation_definition self ctx operation =
    let () = visit_variable_definitions self ctx operation.o_variable_definitions in
    let () = visit_directives self ctx operation.o_directives in
    visit_selection_set self ctx operation.o_selection_set

  let visit_fragment_definition self ctx fragment = 
    let () = visit_directives self ctx fragment.fg_directives in
    visit_selection_set self ctx fragment.fg_selection_set

  let visit_definition self ctx def =
    let def_type = Schema.NonNull(Schema.Named(match def with
        | Operation { item = { o_type = Query } } -> Schema.query_type ctx.schema |> Schema.type_name
        | Operation { item = { o_type = Mutation } } -> Schema.mutation_type ctx.schema |> Option.unsafe_unwrap |> Schema.type_name
        | Operation { item = { o_type = Subscription } } -> Schema.subscription_type ctx.schema |> Option.unsafe_unwrap |> Schema.type_name
        | Fragment { item = { fg_type_condition = { item } } } -> item))
    in
    let ctx = Context.push_type ctx (Some def_type) in
    match def with
    | Operation operation ->
      let () = V.enter_operation_definition self ctx operation in
      let () = visit_operation_definition self ctx operation.item in
      V.exit_operation_definition self ctx operation
    | Fragment fragment ->
      let () = V.enter_fragment_definition self ctx fragment in
      let () = visit_fragment_definition self ctx fragment.item in
      V.exit_fragment_definition self ctx fragment

  let visit_document ctx doc =
    let self = V.make_self () in
    let () = V.enter_document self ctx doc in
    let () = List.iter (visit_definition self ctx) doc in
    let () = V.exit_document self ctx doc in
    self
end

let find_fragments doc =
  let open Graphql_ast in
  let open Source_pos in
  let lookup = Hashtbl.create 1 in
  let () = List.iter (function 
      | Fragment fragment -> Hashtbl.add lookup fragment.item.fg_name.item fragment.item
      | _ -> ()) doc in
  lookup

let make_context config document = {
  map_loc = config.Generator_utils.map_loc;
  fragments = find_fragments document;
  schema = config.Generator_utils.schema;
  errors = ref([]);
  type_stack = [];
  type_literal_stack = [];
  input_type_stack = [];
  input_type_literal_stack = [];
  parent_type_stack = [];
}