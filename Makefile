DUNE?=dune

OS:=$(shell uname -s)
TARGET_BUCKLESCRIPT:=$(findstring $(shell opam switch show),4.02.3)

build:
	$(DUNE) build src/graphql_ppx_bucklescript.exe
	# if [ "$(TARGET_BUCKLESCRIPT)" = "" ]; then  $(DUNE) build src/graphql_ppx_native.a; fi  ## FIXME: Native library does not build
	cp _build/default/src/graphql_ppx_bucklescript.exe graphql_ppx.exe

buildall:
	# $(DUNE) build --workspace=dune-workspace.dev-native src/graphql_ppx_native.a ## FIXME: Native library does not build
	$(DUNE) build --workspace=dune-workspace.dev-bs src/graphql_ppx_bucklescript.exe
	([ -x _build/4.02.3/src/graphql_ppx_bucklescript.exe ] \
		&& cp _build/4.02.3/src/graphql_ppx_bucklescript.exe graphql_ppx.exe)

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
