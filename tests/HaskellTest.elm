module HaskellTest exposing (sourceTests)

import Expect exposing (..)
import Haskell exposing (..)
import Test exposing (..)
import Type exposing (nodeToString)


sourceTests =
    let
        f =
            Var (TypeVar "f")

        a =
            Var (TypeVar "a")

        b =
            Var (TypeVar "b")
    in
    describe "Source forms"
        [ test "nested Apps" <|
            \() ->
                app f [ a, b ] |> Expect.equal (App (App f a) b)
        , test "curried app needs no parens" <|
            \() ->
                (nodeToString << Tuple.second << typeToSrc) (app f [ a, b ])
                    |> Expect.equal "f a b"
        ]
