type scalars = {
  string: string,
  int: int
};

type dog = {
  name: string,
  barkVolume: float
};

module MyQuery = [%graphql {|
  {
    variousScalars @bsRecord {
      string
      int
    }
  }
|}];

module ExternalFragmentQuery = [%graphql {|
  fragment Fragment on VariousScalars @bsRecord {
    string
    int
  }

  {
    variousScalars {
      ...Fragment
    }
  }
|}];

module InlineFragmentQuery = [%graphql {|
  {
    dogOrHuman {
      ...on Dog @bsRecord {
        name
        barkVolume
      }
    }
  }
|}];

module UnionExternalFragmentQuery = [%graphql {|
  fragment DogFragment on Dog @bsRecord {
    name
    barkVolume
  }

  {
    dogOrHuman {
      ...on Dog {
        ...DogFragment
      }
    }
  }
|}];

Jest.(describe("Record", () => {
  open Expect;
  open! Expect.Operators;

  test("Decodes a record in a selection", () => {
    let expected = {string: "a string", int: 123};
    expect(MyQuery.parse(Js.Json.parseExn({|{"variousScalars": {"string": "a string", "int": 123}}|})))
      == {"variousScalars": expected};
  });

  test("Decodes a record in an external fragment", () => {
    let expected = {string: "a string", int: 123};
    expect(ExternalFragmentQuery.parse(Js.Json.parseExn({|{"variousScalars": {"string": "a string", "int": 123}}|})))
      == {"variousScalars": expected};
  });

  test("Decodes a record in an inline fragment", () => {
    let expected = `Dog({name: "name", barkVolume: 123.0});
    expect(InlineFragmentQuery.parse(Js.Json.parseExn({|{"dogOrHuman": {"__typename": "Dog", "name": "name", "barkVolume": 123}}|})))
      == {"dogOrHuman": expected};
  });

  test("Decodes a record in an external fragment on union selections", () => {
    let expected = `Dog({name: "name", barkVolume: 123.0});
    expect(UnionExternalFragmentQuery.parse(Js.Json.parseExn({|{"dogOrHuman": {"__typename": "Dog", "name": "name", "barkVolume": 123}}|})))
      == {"dogOrHuman": expected};
  });
}));
