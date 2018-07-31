type scalars = {
  string: string,
  int: int
};

module MyQuery = [%graphql {|
  {
    variousScalars @bsRecord {
      string
      int
    }
  }
|}];

Jest.(describe("Record", () => {
  open Expect;
  open! Expect.Operators;

  test("Decodes a record", () => {
    let expected = {string: "a string", int: 123};
    expect(MyQuery.parse(Js.Json.parseExn({|{"variousScalars": {"string": "a string", "int": 123}}|})))
      == {"variousScalars": expected};
  });
}));
