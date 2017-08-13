#!/usr/bin/env node

var request = require('request');
var fs = require('fs');

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

if (process.argv.length !== 3) {
    console.error(`Usage: ${process.argv[1]} <API_URL>`);
    process.exit(1);
}

request.post(
    process.argv[2],
    { method: 'POST', json: true, body: { query: introspectionQuery } },
    function (error, response, body) {
        if (error) {
            console.error('Could not send introspection query: ', error);
            process.exit(1);
        }

        if (response.statusCode !== 200) {
            console.error('Non-ok status code from API: ', response.statusCode, response.statusMessage);
            process.exit(1);
        }

        var result = JSON.stringify(body, null, 2);

        fs.writeFileSync('graphql_schema.json', result, { encoding: 'utf-8' });
    });
