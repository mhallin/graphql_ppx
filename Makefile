DUNE?=dune

OS:=$(shell uname -s)

build: src/dune.flags
	$(DUNE) build @graphql_ppx
	cp _build/default/src/graphql_ppx.exe .

test: build tests/graphql_schema.json
	(cd tests && \
		../node_modules/.bin/bsb -clean-world && \
		../node_modules/.bin/bsb -make-world)

tests/graphql_schema.json: tests/schema.gql
	node ./node_modules/gql-tools/cli/gqlschema.js -o tests/graphql_schema.json tests/schema.gql

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
