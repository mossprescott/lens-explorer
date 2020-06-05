#! /bin/bash
set -e

elm-format --yes src/ tests/
elm make src/main.elm --docs=build/documentation.json --output=build/index.html
elm-test
