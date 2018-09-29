#!/bin/sh
set -ex
export POSIXLY_CORRECT=1

hlint .
cabal install -j --enable-tests
cabal haddock --executables
cabal test
