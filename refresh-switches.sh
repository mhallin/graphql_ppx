#!/bin/sh

set -e

SWITCHES="4.02.3 4.04.2 4.05.0 4.06.1 4.07.1"
ORIGINAL_SWITCH=$(opam switch show)

for switch in $SWITCHES; do
    echo "## REFRESHING $switch ##"
    opam switch $switch
    eval $(opam env)
    opam pin add graphql_ppx_base . -n --kind=path -y
    OPAMYES=1 opam install graphql_ppx_base --deps-only
    if [ "$switch" != "4.02.3" ]; then
        opam pin add graphql_ppx . -n --kind=path -y
        OPAMYES=1 opam install graphql_ppx --deps-only
    fi
done

opam switch $ORIGINAL_SWITCH
eval $(opam env)
