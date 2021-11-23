#!/bin/bash
futhark opencl fut/entries.fut --library --verbose
futhask opencl fut/entries.h src Futhark

# stack clean
stack build
# cabal v2-build
# cabal v2-run $1
# stack install
# stack run $1
