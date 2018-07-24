#!/bin/sh

eval `opam config env`

OPAMYES=1 opam pin add graphql_ppx . -n
OPAMYES=1 opam install graphql_ppx --deps-only
