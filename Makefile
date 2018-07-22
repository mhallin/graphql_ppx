DUNE?=dune

OS:=$(shell uname -s)
SKIP_APOLLO_TESTS?=false

build: src/dune.flags
	$(DUNE) build @graphql_ppx
	cp _build/default/src/graphql_ppx.exe .

test: build tests/graphql_schema.json tests_apollo/graphql_schema.json
	# $(DUNE) build @all_tests
	# ./_build/default/tests/all_tests.exe
	(cd tests && ../node_modules/.bin/bsb -clean-world && ../node_modules/.bin/bsb -make-world)
	$(SKIP_APOLLO_TESTS) || (cd tests_apollo && ../node_modules/.bin/bsb -clean-world && ../node_modules/.bin/bsb -make-world)
	$(SKIP_APOLLO_TESTS) || ./node_modules/.bin/jest --verbose tests/lib/js tests_apollo/lib/js

tests/graphql_schema.json: tests/schema.gql
	node ./node_modules/gql-tools/cli/gqlschema.js -o tests/graphql_schema.json tests/schema.gql

tests_apollo/graphql_schema.json: tests/graphql_schema.json
	cp $< $@

ifeq ($(OS),Linux)
src/dune.flags:
	echo '(-ccopt -static)' > $@
else
src/dune.flags:
	echo '()' > $@
endif

clean:
	$(DUNE) clean
	rm -rf _build graphql_ppx.exe tests/lib

.PHONY: build test clean
