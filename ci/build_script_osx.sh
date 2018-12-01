#!/bin/sh

set -e

eval `opam config env`

make
