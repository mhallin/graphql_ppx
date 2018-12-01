DUNE?=dune

OS:=$(shell uname -s)
TARGET_BUCKLESCRIPT?=$(shell echo `opam switch show` | grep -s 4.02.3)

build:
	$(DUNE) build src/bucklescript/graphql_ppx.exe
	if [ "$(TARGET_BUCKLESCRIPT)" = "" ]; then $(DUNE) build src/native/graphql_ppx.a; fi
	cp _build/default/src/bucklescript/graphql_ppx.exe .

buildall:
	$(DUNE) build --workspace=dune-workspace.dev-native src/native/graphql_ppx.a
	$(DUNE) build --workspace=dune-workspace.dev-bs src/bucklescript/graphql_ppx.exe
	([ -x _build/4.02.3/src/bucklescript/graphql_ppx.exe ] \
		&& cp _build/4.02.3/src/bucklescript/graphql_ppx.exe .)

test: build only-test

only-test: graphql_schema.json
	if [ "$(TARGET_BUCKLESCRIPT)" != "" ]; then (cd tests_bucklescript && ../node_modules/.bin/bsb -clean-world && ../node_modules/.bin/bsb -make-world); fi
	if [ "$(TARGET_BUCKLESCRIPT)" != "" ]; then (cd tests_apollo && ../node_modules/.bin/bsb -clean-world && ../node_modules/.bin/bsb -make-world); fi
	if [ "$(TARGET_BUCKLESCRIPT)" != "" ]; then ./node_modules/.bin/jest --verbose tests_bucklescript/lib/js tests_apollo/lib/js; fi
	if [ "$(TARGET_BUCKLESCRIPT)" = "" ]; then dune runtest; fi

graphql_schema.json: schema.gql
	node ./node_modules/gql-tools/cli/gqlschema.js -o $@ $<

clean:
	$(DUNE) clean
	rm -rf _build graphql_ppx.exe tests_bucklescript/lib tests_apollo/lib

.PHONY: build test only-test clean buildall
