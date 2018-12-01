#!/bin/bash

set -eu
set +x

echo "Agent OS: $AGENT_OS"
echo "OCaml version: $OCAML_VERSION"

export BUCKLESCRIPT_OCAML="4.02.3"

if [ "$OCAML_VERSION" = "$BUCKLESCRIPT_OCAML" ]; then
    export TARGET_BUCKLESCRIPT=1
else
    export TARGET_BUCKLESCRIPT=
fi

case "$AGENT_OS" in
    Darwin)
        export TARGET_NAME=darwin-x64
        ;;
    Linux)
        export TARGET_NAME=linux-x64
        ;;
esac

echo "graphql_ppx target name: $TARGET_NAME"
echo "Targeting bucklescript: $TARGET_BUCKLESCRIPT"

case "$AGENT_OS" in
    Darwin)
        brew update
        brew unlink python
        brew install aspcud awscli yarn opam ocaml || true

        OPAMYES=1 opam init
        OPAMYES=1 opam switch create $OCAML_VERSION
        OPAMYES=1 opam switch $OCAML_VERSION

        eval $(opam env)

        OPAMYES=1 opam update
        OPAMYES=1 opam pin add graphql_ppx_base . -n
        OPAMYES=1 opam install graphql_ppx_base --deps-only --with-test

        if [ "$TARGET_BUCKLESCRIPT" != "1" ]; then
            OPAMYES=1 opam pin add graphql_ppx . -n
            OPAMYES=1 opam install graphql_ppx --deps-only --with-test
        fi

        make
        ;;

    Linux)

        if [ "$TARGET_BUCKLESCRIPT" = "1" ]; then
            chmod -R a+w .
            docker run --rm -v `pwd`:/workspace ocaml/opam2:alpine sh -c "\
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
            sudo apt-get update -y
            sudo apt-get install -y git
            echo | sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
            OPAMYES=1 opam init --disable-sandboxing --bare
            OPAMYES=1 opam switch create $OCAML_VERSION
            OPAMYES=1 opam switch $OCAML_VERSION
            eval $(opam env)
            OPAMYES=1 opam update
            OPAMYES=1 opam pin add graphql_ppx_base . -n
            OPAMYES=1 opam install graphql_ppx_base --deps-only --with-test
            OPAMYES=1 opam pin add graphql_ppx . -n
            OPAMYES=1 opam install graphql_ppx --deps-only --with-test
        fi
        ;;
esac

if [ "$TARGET_BUCKLESCRIPT" = "1" ]; then
    IS_GRAPHQL_PPX_CI=true yarn
fi

make only-test

if [ "$TARGET_BUCKLESCRIPT" = "1" ]; then
    NODE_ENV=production make only-test
    mv graphql_ppx.exe graphql_ppx-$TARGET_NAME.exe
fi

