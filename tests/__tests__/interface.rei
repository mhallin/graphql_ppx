module QueryWithFragments: {
  type t = Js.t({
    .
    users: array([
      | `User(Js.t({ . id: string }))
      | `AdminUser(Js.t({ . id: string, name: string }))
      | `AnonymousUser(Js.t({ . id: string, anonymousId: int }))
    ])
  });

  let make: unit => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });
  let makeWithVariables: Js.t({.}) => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });
  let query: string;
};

module QueryWithoutFragments: {
  type t = Js.t({
    .
    users: array([
      | `User(Js.t({ . id: string }))
    ])
  });

  let make: unit => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });
  let makeWithVariables: Js.t({.}) => Js.t({ . parse: Js.Json.t => t, query: string, variables: Js.Json.t });
  let query: string;
};
