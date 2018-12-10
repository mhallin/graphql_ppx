type output_mode =
  | String
  | Apollo_AST

type config = {
  verbose_logging: bool;
  output_mode: output_mode;
  verbose_error_handling: bool;
  apollo_mode: bool;
  root_directory: string;
  schema_file: string;
  raise_error_with_loc: 'a. Source_pos.ast_location -> string -> 'a
}

let config_ref = ref None

let set_config config = config_ref := Some(config)

let verbose_logging () =
  (!config_ref |> Option.unsafe_unwrap).verbose_logging

let output_mode () =
  (!config_ref |> Option.unsafe_unwrap).output_mode

let apollo_mode () =
  (!config_ref |> Option.unsafe_unwrap).apollo_mode

let verbose_error_handling () =
  (!config_ref |> Option.unsafe_unwrap).verbose_error_handling

let root_directory () =
  (!config_ref |> Option.unsafe_unwrap).root_directory

let schema_file () =
  (!config_ref |> Option.unsafe_unwrap).schema_file

let raise_error_with_loc loc message =
  (!config_ref |> Option.unsafe_unwrap).raise_error_with_loc loc message
