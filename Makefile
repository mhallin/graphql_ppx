PREFIX?=$(shell opam config var prefix)
OCAMLFIND_IGNORE_DUPS_IN = $(shell ocamlfind query compiler-libs)
export OCAMLFIND_IGNORE_DUPS_IN

build:
	ocamlbuild -use-ocamlfind \
		-package ocaml-migrate-parsetree \
		-package result \
		-package yojson \
		-package ppx_tools.metaquot \
		-I src \
		graphql_ppx.native

test: build tests/graphql_schema.json
	(cd tests && \
		../node_modules/.bin/bsb -clean-world && \
		../node_modules/.bin/bsb -make-world)

tests/graphql_schema.json: tests/schema.gql
	node ./node_modules/gql-tools/cli/gqlschema.js -o tests/graphql_schema.json tests/schema.gql

install: build
	@opam-installer --prefix=$(PREFIX) graphql_ppx.install

uninstall:
	@opam-installer -u --prefix=$(PREFIX) graphql_ppx.install

clean:
	rm -rf _build graphql_ppx.native tests/lib

.PHONY: build test clean
