#!/usr/bin/env bash
cabal build && ./maelstrom/maelstrom test -w unique-ids --bin dist-newstyle/build/aarch64-osx/ghc-9.6.2/kurimus-0.1.0.0/x/kurimus/build/kurimus/kurimus --time-limit 30 --rate 1000 --node-count 3 --availability total --nemesis partition
