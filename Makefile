JBUILDER?=jbuilder

OS:=$(shell uname -s)

build: src/jbuild.flags
	$(JBUILDER) build @graphql_ppx
	cp _build/default/src/graphql_ppx.exe .

test: build tests/graphql_schema.json
	(cd tests && \
		../node_modules/.bin/bsb -clean-world && \
		../node_modules/.bin/bsb -make-world)

tests/graphql_schema.json: tests/schema.gql
	node ./node_modules/gql-tools/cli/gqlschema.js -o tests/graphql_schema.json tests/schema.gql

ifeq ($(OS),Linux)
src/jbuild.flags:
	echo '(-ccopt -static)' > $@
else
src/jbuild.flags:
	echo '()' > $@
endif

clean:
	$(JBUILDER) clean
	rm -rf _build graphql_ppx.exe tests/lib

.PHONY: build test clean
