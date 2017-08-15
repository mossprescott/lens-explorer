module FunctionsTest exposing (..)

import Expect exposing (..)
import Test exposing (..)
import Haskell exposing (..)
import Type exposing (nodeToString)
import Functions exposing (..)
import Dict


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
