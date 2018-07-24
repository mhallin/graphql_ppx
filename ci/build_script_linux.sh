#!/bin/sh

chmod -R a+w .
docker run --rm -v `pwd`:/home/opam/workspace -it ocaml/opam:alpine_ocaml-4.02.3 sh -c "\
    sudo apk add m4 && \
    (cd opam-repository && git pull --quiet) && \
    OPAMYES=1 opam update && \
    (cd workspace && \
        OPAMYES=1 opam pin add graphql_ppx . -n && \
        OPAMYES=1 opam install graphql_ppx --deps-only && \
        make \
    )"
