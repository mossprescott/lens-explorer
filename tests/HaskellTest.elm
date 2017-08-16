module HaskellTest exposing (..)

import Expect exposing (..)
import Test exposing (..)
import Haskell exposing (..)
import Type exposing (nodeToString)
import Lens exposing (lens, opticType, orSame, simplify)


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
            [ test "nested Apps" <|
                \() ->
                    app f [ a, b ] |> Expect.equal (App (App f a) b)
            , test "curried app needs no parens" <|
                \() ->
                    (nodeToString << Tuple.second << typeToSrc) (app f [ a, b ])
                        |> Expect.equal "f a b"
            ]


composeTests =
    let
        simpleLens =
            (orSame simplify) lens

        s =
            TypeVar { name = "s" }

        a =
            TypeVar { name = "a" }

        b =
            TypeVar { name = "b" }

        c =
            TypeVar { name = "c" }
    in
        describe "composeFns"
            [ test "simple Lenses" <|
                \() ->
                    composeFns (opticType simpleLens) (opticType simpleLens)
                        |> Expect.equal
                            (Ok
                                ( [ ( s, a ), ( a, b ) ]
                                , [ ( s, b ), ( a, c ) ]
                                , substitute [ ( s, a ), ( a, c ) ] (opticType simpleLens)
                                )
                            )
            ]
