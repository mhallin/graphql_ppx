PREFIX?=$(shell opam config var prefix)

build:
	ocamlbuild -package ocaml-migrate-parsetree -package result -package yojson -I src graphql_ppx.native

test: build
	(cd tests && \
		ocamlfind ppx_tools/rewriter ../graphql_ppx.native test.ml && \
		../node_modules/.bin/bsb && \
		node lib/js/test.js)

install: build
	@opam-installer --prefix=$(PREFIX) graphql_ppx.install

uninstall:
	@opam-installer -u --prefix=$(PREFIX) graphql_ppx.install

clean:
	rm -rf _build graphql_ppx.native

.PHONY: build test clean
