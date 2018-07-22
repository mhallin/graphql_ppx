type record = {f1: string, f2: string};

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
        inner @bsRecord {
          f1: field
          f2: field
        }
      }
    }
  }
|}];

Jest.(describe("Nested", () => {
  open Expect;
  open! Expect.Operators;

  test("Decodes recursively", () =>
    expect(MyQuery.parse(Js.Json.parseExn({|
      {"first": {"inner": {"inner": {"field": "second"}}},
       "second": {"inner": null}}
    |}))) == {
      "first": {"inner": Some({"inner": Some({"field": "second"})})},
      "second": {"inner": None}
    });
}));
