elm-make src/main.elm --output=main.html
elm-make test/LensTest.elm --output=build/LensTest.js
node build/LensTest.js
