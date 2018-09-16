module MyQuery: {
  type t = {
    .
    "users": [
      | `AnonymousUser({. "id": string})
      | `AdminUser(
          {
            .
            "id": string,
            "name": string,
          },
        )
    ],
  };

  let make:
    unit =>
    {
      .
      "parse": Js.Json.t => t,
      "query": string,
      "variables": Js.Json.t,
    };
  let makeWithVariables:
    Js.t({.}) =>
    {
      .
      "parse": Js.Json.t => t,
      "query": string,
      "variables": Js.Json.t,
    };
  let query: string;
};