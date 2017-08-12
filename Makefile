build:
	ocamlbuild -package ocaml-migrate-parsetree -package result -package yojson -I lib graphql_ppx.native

test: build
	(cd tests && ocamlfind ppx_tools/rewriter ../graphql_ppx.native test.ml)
#	ocamlbuild -package ounit -I tests test.native
#	./test.native

clean:
	rm -rf _build graphql_ppx.native

.PHONY: build test clean
