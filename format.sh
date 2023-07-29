#!/usr/bin/env bash

# This script uses ormolu to format Haskell source files inplace.

# Format the files in the "test" directory
ormolu -m inplace test/*.hs

# Format the files in the "app" directory
ormolu -m inplace app/*.hs

# Format the files in the "src" directory
ormolu -m inplace src/*.hs
