module MyQuery = [%graphql
  {|
    query pokemon($id: String, $name: String) {
      pokemon(name: $name, id: $id) {
        id
        name
      }
    }
  |};
  "pokedex_schema.json"
];

Jest.(
  describe("Scalars as arguments", () => {
    open Expect;
    open! Expect.Operators;

    test("Allows you to omit nullable arguments", () =>
      expect(MyQuery.make(~name="Pikachu", ())##variables)
      == Js.Json.parseExn(
           {| {
             "name": "Pikachu",
             "id": null
           } |},
         )
    );

    test("Includes non-nulled arguments", () =>
      expect(MyQuery.make(~id="pikachu_id", ~name="Pikachu", ())##variables)
      == Js.Json.parseExn(
           {| {
             "name": "Pikachu",
             "id": "pikachu_id"
           } |},
         )
    );
  })
);
