PREFIX?=$(shell opam config var prefix)

build:
	ocamlbuild -package ocaml-migrate-parsetree -package result -package yojson -I src graphql_ppx.native

test: build
	(cd tests && \
		../node_modules/.bin/bsb)

install: build
	@opam-installer --prefix=$(PREFIX) graphql_ppx.install

uninstall:
	@opam-installer -u --prefix=$(PREFIX) graphql_ppx.install

clean:
	rm -rf _build graphql_ppx.native test/lib

.PHONY: build test clean
