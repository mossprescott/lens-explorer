module LensTest exposing (..)

import Expect exposing (..)
import Test exposing (..)
import Haskell exposing (..)
import Lens exposing (..)
import Library exposing (..)
import Type exposing (nodeToString)


typeTests =
    describe "The basic type of each optic, transcribed from the haddock"
        [ test "Lens" <|
            \() ->
                (orSame irregular << opticType) lens
                    |> Expect.equal
                        (Constrained [ TypeClassConstraint functor [ f ] ]
                            (Fn
                                (Fn (Var a) (App (Var f) (Var b)))
                                (Fn (Var s) (App (Var f) (Var t)))
                            )
                        )
        , test "Iso" <|
            \() ->
                opticType iso
                    |> Expect.equal
                        (Constrained [ TypeClassConstraint profunctor [ p ], TypeClassConstraint functor [ f ] ]
                            (Fn
                                (App (App (Var p) (Var a)) (App (Var f) (Var b)))
                                (App (App (Var p) (Var s)) (App (Var f) (Var t)))
                            )
                        )
        , test "Prism" <|
            \() ->
                opticType prism
                    |> Expect.equal
                        (Constrained [ TypeClassConstraint choice [ p ], TypeClassConstraint applicative [ f ] ]
                            (Fn
                                (App (App (Var p) (Var a)) (App (Var f) (Var b)))
                                (App (App (Var p) (Var s)) (App (Var f) (Var t)))
                            )
                        )
        , test "Traversal" <|
            \() ->
                (orSame irregular << opticType) traversal
                    |> Expect.equal
                        (Constrained [ TypeClassConstraint applicative [ f ] ]
                            (Fn
                                (Fn (Var a) (App (Var f) (Var b)))
                                (Fn (Var s) (App (Var f) (Var t)))
                            )
                        )
        , test "Fold" <|
            \() ->
                (orSame irregular << opticType) fold
                    |> Expect.equal
                        (Constrained [ TypeClassConstraint contravariant [ f ], TypeClassConstraint applicative [ f ] ]
                            (Fn
                                (Fn (Var a) (App (Var f) (Var a)))
                                (Fn (Var s) (App (Var f) (Var s)))
                            )
                        )
        , test "Fold1" <|
            \() ->
                (orSame irregular << opticType) fold1
                    |> Expect.equal
                        (Constrained [ TypeClassConstraint contravariant [ f ], TypeClassConstraint apply [ f ] ]
                            (Fn
                                (Fn (Var a) (App (Var f) (Var a)))
                                (Fn (Var s) (App (Var f) (Var s)))
                            )
                        )
        ]


sourceTests =
    describe "Source forms"
        [ test "Iso" <|
            \() ->
                (nodeToString << opticToSrc identity) iso
                    |> Expect.equal "type Iso s t a b = forall p f . (Profunctor p, Functor f) ⇒ p a (f b) → p s (f t)"
        ]


composeTests =
    let
        x =
            TypeVar "x"

        y =
            TypeVar "y"

        z =
            TypeVar "z"
    in
        describe "compose"
            [ test "two simple lenses" <|
                \() ->
                    Maybe.map2 compose (simplify lens) (simplify lens)
                        |> Expect.equal
                            (Just
                                -- Note: these all just substitutions on simpleLens
                                ( Optic "Lens'"
                                    [ x, y ]
                                    []
                                    []
                                    [ functor ]
                                    (Fn (Var y) (App (Var f) (Var y)))
                                    (Fn (Var x) (App (Var f) (Var x)))
                                , Optic "Lens'"
                                    [ y, z ]
                                    []
                                    []
                                    [ functor ]
                                    (Fn (Var z) (App (Var f) (Var z)))
                                    (Fn (Var y) (App (Var f) (Var y)))
                                , Optic ""
                                    [ s, a ]
                                    []
                                    []
                                    [ functor ]
                                    (Fn (Var z) (App (Var f) (Var z)))
                                    (Fn (Var x) (App (Var f) (Var x)))
                                )
                            )
            ]
