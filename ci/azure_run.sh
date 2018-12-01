#!/bin/bash

set -eu

echo "Agent OS: $AGENT_OS"
echo "OCaml version: $OCAML_VERSION"

export BUCKLESCRIPT_OCAML="4.02.3"

[ "$OCAML_VERSION" = "$BUCKLESCRIPT_OCAML" ]
export TARGET_BUCKLESCRIPT=$?

case "$AGENT_OS" in
    Darwin)
        export TARGET_NAME=darwin-x64
        ;;
    Linux)
        export TARGET_NAME=linux-x64
        ;;
esac

echo "graphql_ppx target name: $TARGET_NAME"

if $TARGET_BUCKLESCRIPT; then
    IS_GRAPHQL_PPX_CI=true yarn
fi

case "$AGENT_OS" in
    Darwin)
        brew update
        brew unlink python
        brew install aspcud awscli yarn opam ocaml
        curl -L https://github.com/aktau/github-release/releases/download/v0.7.2/darwin-amd64-github-release.tar.bz2 | tar xjf -
        mv bin/darwin/amd64/github-release .

        OPAMYES=1 opam init
        OPAMYES=1 opam switch create $OCAML_VERSION
        OPAMYES=1 opam switch $OCAML_VERSION

        eval $(opam env)

        OPAMYES=1 opam update
        OPAMYES=1 opam pin add graphql_ppx_base . -n
        OPAMYES=1 opam install graphql_ppx_base --deps-only

        if ! $TARGET_BUCKLESCRIPT; then
            OPAMYES=1 opam pin add graphql_ppx . -n
            OPAMYES=1 opam install graphql_ppx --deps-only
        fi

        make
        ;;

    Linux)

        if $TARGET_BUCKLESCRIPT; then
            chmod -R a+w .
            docker run --rm -v `pwd`:/workspace -it ocaml/opam2:alpine sh -c "\
                sudo apk add m4 && \
                OPAMYES=1 opam switch create $OCAML_VERSION &&
                OPAMYES=1 opam switch $OCAML_VERSION &&
                eval \$(opam env) &&
                OPAMYES=1 opam update && \
                (cd /workspace && \
                    OPAMYES=1 opam pin add graphql_ppx_base . -n && \
                    OPAMYES=1 opam install graphql_ppx_base --deps-only && \
                    make \
                )"
        else
            sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
            OPAMYES=1 opam switch create $OCAML_VERSION
            OPAMYES=1 opam switch $OCAML_VERSION
            eval $(opam env)
            OPAMYES=1 opam update
            OPAMYES=1 opam pin add graphql_ppx_base . -n
            OPAMYES=1 opam install graphql_ppx_base --deps-only
            OPAMYES=1 opam pin add graphql_ppx . -n
            OPAMYES=1 opam install graphql_ppx --deps-only
        fi
        ;;
esac

make only-test

if $TARGET_BUCKLESCRIPT; then
    NODE_ENV=production make only-test
fi

mv graphql_ppx.exe graphql_ppx-$TARGET_NAME.exe
