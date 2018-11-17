#!/bin/sh
set -ex
export POSIXLY_CORRECT=1

hlint .
stack haddock
stack test
