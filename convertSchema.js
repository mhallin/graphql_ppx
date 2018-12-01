#!/usr/bin/env node
const graphqlTools = require('graphql-tools');
const graphql = require('graphql');
const fs = require('fs');

const argv = require("yargs")
  .usage('Usage: $0 <schema>')
  .command("schema", "Path to the schema file", { alias: "schema" })
  .required(1, "The schema file is required")
  .help("?")
  .alias("?", "help").argv;

const introspectionQuery = `
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

const typeDefs = fs.readFileSync(argv._[0], {encoding: 'utf-8'});
const schema = graphqlTools.makeExecutableSchema({ typeDefs, resolverValidationOptions: { requireResolversForResolveType: false } });
graphqlTools.addMockFunctionsToSchema({ schema });

graphql.graphql(schema, introspectionQuery).then(result => 
    process.stdout.write(JSON.stringify(result, null, '  ')));
