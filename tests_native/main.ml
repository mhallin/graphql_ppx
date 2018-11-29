let () = Alcotest.run "Native PPX tests" [
    "Argument named 'query'", Arg_named_query.tests;
    "Custom decoder", Custom_decoder.tests;
    "Custom scalars", Custom_scalars.tests;
    "Enum input", Enum_input.tests;
    "Fragment definition", Fragment_definition.tests;
    "Interface", Interface.tests;
    "Lists", Lists.tests;
    "List arguments", List_args.tests;
    "List inputs", List_inputs.tests;
    "Mutations", Mutation.tests;
    "Nested decoding", Nested.tests;
  ]
