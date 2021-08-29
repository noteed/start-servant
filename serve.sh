#! /usr/bin/env bash

runghc -isrc/ bin/serve.hs

# consider switching to
# cabal run --disable-optimization exe:start-servant
