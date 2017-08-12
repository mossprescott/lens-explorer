module HaskellTest exposing (..)

import Expect exposing (..)
import Test exposing (..)
import Haskell exposing (..)
import Type exposing (nodeToString)


sourceTests =
    let
        f =
            Var (TypeVar { name = "f" })

        a =
            Var (TypeVar { name = "a" })

        b =
            Var (TypeVar { name = "b" })
    in
        describe "Source forms"
            [ test "curried app needs no parens" <|
                \() ->
                    (nodeToString << Tuple.second << typeToSrc) (App (App f a) b)
                        |> Expect.equal "f a b"
            ]
