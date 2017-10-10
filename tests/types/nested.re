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