module FunctionsTest exposing (sourceTests)

import Dict
import Expect exposing (..)
import Haskell exposing (..)
import Lens.Functions exposing (..)
import Test exposing (..)
import Type exposing (nodeToString)


sourceTests =
    let
        fnMap =
            Dict.fromList functions

        forName n =
            Dict.get n fnMap

        toSrcString =
            nodeToString << Tuple.second << typeToSrc
    in
    describe "Source forms"
        [ test "view" <|
            \() ->
                Maybe.map toSrcString (forName "view")
                    |> Expect.equal (Just "(MonadReader s m) ⇒ Getting a s a → m a")
        ]
