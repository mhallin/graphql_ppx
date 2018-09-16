module MyQuery = [%graphql
  {|
   query {
    users {
      id
      ... on AdminUser {
        name
      }
    }
  }
|}
];

Jest.(
  describe("Interface definition", () => {
    open Expect;
    open! Expect.Operators;

    test("Decodes the interface", () =>
      expect(
        MyQuery.parse(
          Js.Json.parseExn(
            {|{
               "users": [
                { "__typename": "AdminUser", "id": "1", "name": "bob" },
                { "__typename": "AnonymousUser", "id": "2"}
            ]}|},
          ),
        ),
      )
      == {
           "users": [|
             `AdminUser({"id": "1", "name": "bob"}),
             `AnonymousUser({"id": "2"}),
           |],
         }
    );
  })
);