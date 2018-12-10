
module MyQuery = [%graphql {|
  {
    first: nestedObject {
      inner {
        inner {
          field
        }
      }
    }

    second: nestedObject {
      inner {
        inner {
          f1: field
          f2: field
        }
      }
    }
  }
|}];

Jest.(describe("Apollo mode", () => {
  open Expect;
  open! Expect.Operators;

  test("Adds __typename to objects", () => {
    let typenameRegex = [%bs.re {|/__typename/g|}];
    Js.log(MyQuery.query)
    MyQuery.query
    |> Js.String.match(typenameRegex)
    |> Belt.Option.map(_, Array.length)
    |> expect == Some(7);
  });
}));
