# Developing

This document describes how to develop and test this ppx plugin.
This is a different workflow than when consuming this ppx plugin for
use in npm. It involves installing native opam packages as
dependencies.


## Developing With `opam`

OPAM is a package and environment manager for OCaml. To work on graphql_ppx
using OPAM, you first need to make sure you're running on OCaml version 4.02.3:

```sh
opam init
opam switch 4.02.3
eval `opam config env`
```

Then, you can install and build graphql_ppx:

```sh
opam pin add graphql_ppx . -n
opam install graphql_ppx --deps-only  # Downloads and builds the OCaml dependencies
make  # Builds graphql_ppx itself
```

### Test

You first need to install the JavaScript dependencies to run the test suite:

```sh
npm install  # Or yarn install if you're using Yarn
```

Then, simply run

```sh
make test
```

to run the tests.


## Developing With `esy`

You can work on the GraphQL ppx plugin by installing native
dependencies using `esy` (`esy` is like `npm` for native).

### Install Dependencies And Build

```sh
npm install -g esy@preview
esy install
esy build
```
