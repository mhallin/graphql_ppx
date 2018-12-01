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
    "Nonrecursive input", Nonrecursive_input.tests;
    "Records", Record.tests;
    "Recursive input", Recursive_input.tests;
    "Scalars", Scalars.tests;
    "Scalar arguments", Scalars_args.tests;
    "Scalar inputs", Scalars_input.tests;
    "Skip directives", Skip_directives.tests;
    "Typename", Typename.tests;
    "Unions", Union.tests;
    "Partial unions", Union_partial.tests;
    "Variant conversion", Variant.tests;
  ]
