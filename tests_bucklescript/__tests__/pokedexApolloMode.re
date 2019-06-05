module MyQuery = [%graphql
  {|
    {
      pokemon(name: "Pikachu") {
        id
        name
      }
    }
|};
  "pokedex_schema.json"
];

Jest.(
  describe("Apollo mode with alternate schema", () => {
    open Expect;
    open! Expect.Operators;

    test("Adds __typename to objects", () => {
      let typenameRegex = [%bs.re {|/__typename/g|}];
      MyQuery.query
      |> Js.String.match(typenameRegex)
      |> Belt.Option.map(_, Array.length)
      |> expect == Some(2);
    });
  })
);
