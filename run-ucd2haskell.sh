#!/bin/sh

# `--core-prop` can be used to generate Haskell data structures for any
# *Derived Property* from `ucd/DerivedCoreProperties.txt`

cabal run --flag ucd2haskell ucd2haskell -- \
--input ./ucd \
--output ./lib/Data/Unicode/Properties \
--core-prop Uppercase \
--core-prop Lowercase \
--core-prop Alphabetic
