#!/bin/bash
stack build --ghc-options -O2

# requires elm 0.19.1
elm make elm-src/Feed.elm --output templates/feeds.html --optimize
