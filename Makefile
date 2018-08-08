DUNE?=dune

OS:=$(shell uname -s)

build:
	$(DUNE) build @graphql_ppx
	cp _build/default/src/graphql_ppx.exe .

test: build only-test

only-test: tests/graphql_schema.json tests_apollo/graphql_schema.json
	(cd tests && ../node_modules/.bin/bsb -clean-world && ../node_modules/.bin/bsb -make-world)
	(cd tests_apollo && ../node_modules/.bin/bsb -clean-world && ../node_modules/.bin/bsb -make-world)
	./node_modules/.bin/jest --verbose tests/lib/js tests_apollo/lib/js

tests/graphql_schema.json: tests/schema.gql
	node ./node_modules/gql-tools/cli/gqlschema.js -o tests/graphql_schema.json tests/schema.gql

tests_apollo/graphql_schema.json: tests/graphql_schema.json
	cp $< $@

clean:
	$(DUNE) clean
	rm -rf _build graphql_ppx.exe tests/lib

.PHONY: build test only-test clean
