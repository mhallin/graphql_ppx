module MyQuery = [%graphql {|
  {
    variousScalars {
      string @bsDecoder(fn: "int_of_string")
      int @bsDecoder(fn: "string_of_int")
    }
  }
|}];

Jest.(describe("Custom decoders", () => {
  open Expect;
  open! Expect.Operators;

  test("Runs the decoder", () =>
    expect(MyQuery.parse(Js.Json.parseExn({|{"variousScalars": {"string": "123", "int": 456}}|})))
      == {"variousScalars": {"string": 123, "int": "456"}})
}));
