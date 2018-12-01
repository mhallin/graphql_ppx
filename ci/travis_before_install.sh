#!/bin/bash

set -e

case "$TRAVIS_OS_NAME" in
    osx)
        brew update
        brew unlink python
        brew install aspcud awscli yarn opam ocaml
        curl -L https://github.com/aktau/github-release/releases/download/v0.7.2/darwin-amd64-github-release.tar.bz2 | tar xjf -
        mv bin/darwin/amd64/github-release .

        OPAMYES=1 opam init
        OPAMYES=1 opam switch create 4.02.3
        OPAMYES=1 opam switch 4.02.3

        eval $(opam env)
        ;;
    linux)
        source "$HOME/.nvm/nvm.sh"
        nvm install 9
        nvm use 9
        curl -L https://github.com/aktau/github-release/releases/download/v0.7.2/linux-amd64-github-release.tar.bz2 | tar xjf -
        mv bin/linux/amd64/github-release .
        ;;
esac
