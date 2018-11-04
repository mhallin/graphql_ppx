DUNE?=dune

OS:=$(shell uname -s)
TARGET_BUCKLESCRIPT:=$(findstring $(shell opam switch show),4.02.3)

build:
	$(DUNE) build src/bucklescript/graphql_ppx.exe
	if [ "$(TARGET_BUCKLESCRIPT)" = "" ]; then  $(DUNE) build src/native/graphql_ppx.a; fi
	cp _build/default/src/bucklescript/graphql_ppx.exe .

buildall:
	$(DUNE) build --workspace=dune-workspace.dev-native src/native/graphql_ppx.a
	$(DUNE) build --workspace=dune-workspace.dev-bs src/bucklescript/graphql_ppx.exe
	([ -x _build/4.02.3/src/bucklescript/graphql_ppx.exe ] \
		&& cp _build/4.02.3/src/bucklescript/graphql_ppx.exe .)

test: build only-test

only-test: tests_bucklescript/graphql_schema.json tests_apollo/graphql_schema.json
	(cd tests_bucklescript && ../node_modules/.bin/bsb -clean-world && ../node_modules/.bin/bsb -make-world)
	(cd tests_apollo && ../node_modules/.bin/bsb -clean-world && ../node_modules/.bin/bsb -make-world)
	./node_modules/.bin/jest --verbose tests_bucklescript/lib/js tests_apollo/lib/js

tests_bucklescript/graphql_schema.json: tests_bucklescript/schema.gql
	node ./node_modules/gql-tools/cli/gqlschema.js -o tests_bucklescript/graphql_schema.json tests_bucklescript/schema.gql

tests_apollo/graphql_schema.json: tests_bucklescript/graphql_schema.json
	cp $< $@

clean:
	$(DUNE) clean
	rm -rf _build graphql_ppx.exe tests_bucklescript/lib

.PHONY: build test only-test clean buildall
