#!/usr/bin/env node
var argv = require("yargs")
  .usage('Usage: $0 <url>  [--headers "key:value"]')
  .command("url", "URL of the GraphQL endpoint", { alias: "url" })
  .required(1, "URL is required")
  .option("H", {
    alias: "headers",
    describe: "Additional Headers to send with introspection query",
    type: "array",
    coerce: arg => {
      let additionalHeaders = {};
      for (const header of arg) {
        const separator = header.indexOf(":");
        const name = header.substring(0, separator).trim();
        const value = header.substring(separator + 1).trim();
        if (!(name && value)) {
          throw new Error('Headers should be specified as "Name: Value"');
        }
        additionalHeaders[name] = value;
      }
      return additionalHeaders;
    }
  })
  .help("?")
  .alias("?", "help")
  .example("$0 https://example.com/graphql", "Get GraphQL Schema")
  .example(`$0 https://example.com/graphql --headers "Authorisation: <token>"`, "Get GraphQL Schema with Authorisation header").argv;

var request = require("request");
var fs = require("fs");
var introspectionQuery = `
query IntrospectionQuery {
    __schema {
      queryType { name }
      mutationType { name }
      subscriptionType { name }
      types {
        ...FullType
      }
      directives {
        name
        description
        locations
        args {
          ...InputValue
        }
      }
    }
  }
  fragment FullType on __Type {
    kind
    name
    description
    fields(includeDeprecated: true) {
      name
      description
      args {
        ...InputValue
      }
      type {
        ...TypeRef
      }
      isDeprecated
      deprecationReason
    }
    inputFields {
      ...InputValue
    }
    interfaces {
      ...TypeRef
    }
    enumValues(includeDeprecated: true) {
      name
      description
      isDeprecated
      deprecationReason
    }
    possibleTypes {
      ...TypeRef
    }
  }
  fragment InputValue on __InputValue {
    name
    description
    type { ...TypeRef }
    defaultValue
  }
  fragment TypeRef on __Type {
    kind
    name
    ofType {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
                ofType {
                  kind
                  name
                }
              }
            }
          }
        }
      }
    }
  }`;

const requestOptions = {
  json: true,
  body: { query: introspectionQuery },
  headers: { "user-agent": "node.js", ...argv.headers }
};

request.post(argv._[0], requestOptions, function(error, response, body) {
  if (error) {
    console.error("Could not send introspection query: ", error);
    process.exit(1);
  }

  if (response.statusCode !== 200) {
    console.error("Non-ok status code from API: ", response.statusCode, response.statusMessage);
    process.exit(1);
  }

  var result = JSON.stringify(body, null, 2);

  fs.writeFileSync("graphql_schema.json", result, { encoding: "utf-8" });
});
