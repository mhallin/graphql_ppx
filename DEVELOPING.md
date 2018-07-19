# Developing

This document describes how to develop and test this ppx plugin.
This is a different workflow than when consuming this ppx plugin for
use in npm. It involves installing native opam packages as
dependencies.



### Developing With `esy`

You can develop and test the graphQL ppx plugin by installing native
dependencies using `esy` (`esy` is like `npm` for native).

## Install Dependencies And Build

    npm install -g esy@preview
    esy install
    esy build

## Test

The following tests the binary built in the previous step, using
`bs-platform`:

    npm install   # installs bs-platform via npm
    esy test


### TODO:
- Document other possible workflows (using `opam` directly).
