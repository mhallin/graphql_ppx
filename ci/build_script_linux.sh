#!/bin/sh

chmod -R a+w .
docker run --rm -v `pwd`:/workspace -it ocaml/opam2:alpine sh -c "\
    sudo apk add m4 && \
    OPAMYES=1 opam switch create 4.02.3 &&
    eval \$(opam env) &&
    OPAMYES=1 opam update && \
    (cd /workspace && \
        OPAMYES=1 opam pin add graphql_ppx_base . -n && \
        OPAMYES=1 opam install graphql_ppx_base --deps-only && \
        make \
    )"
