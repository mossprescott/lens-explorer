#! /bin/bash
set -e

elm make src/main.elm --docs=build/documentation.json --output=build/index.html
elm-test
