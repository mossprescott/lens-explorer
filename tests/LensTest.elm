module LensTest exposing (..)

import Expect exposing (..)
import Test exposing (..)
import Haskell exposing (..)
import Lens exposing (..)


tests : Test
tests =
    describe "The basic type of each optic, transcribed from the haddock"
        [ test "Lens" <|
            \() ->
                opticType lens
                    |> Expect.equal
                        (Constrained [ TypeClassConstraint functor f ]
                            (Fn
                                (Fn (Var a) (App (Var f) (Var b)))
                                (Fn (Var s) (App (Var f) (Var t)))
                            )
                        )
        , test "Iso" <|
            \() ->
                opticType iso
                    |> Expect.equal
                        (Constrained [ TypeClassConstraint profunctor p, TypeClassConstraint functor f ]
                            (Fn
                                (App2 (Var p) (Var a) (App (Var f) (Var b)))
                                (App2 (Var p) (Var s) (App (Var f) (Var t)))
                            )
                        )
        , test "Prism" <|
            \() ->
                opticType prism
                    |> Expect.equal
                        (Constrained [ TypeClassConstraint choice p, TypeClassConstraint applicative f ]
                            (Fn
                                (App2 (Var p) (Var a) (App (Var f) (Var b)))
                                (App2 (Var p) (Var s) (App (Var f) (Var t)))
                            )
                        )
        , test "Traversal" <|
            \() ->
                opticType traversal
                    |> Expect.equal
                        (Constrained [ TypeClassConstraint applicative f ]
                            (Fn
                                (Fn (Var a) (App (Var f) (Var b)))
                                (Fn (Var s) (App (Var f) (Var t)))
                            )
                        )
        , test "Fold" <|
            \() ->
                opticType fold
                    |> Expect.equal
                        (Constrained [ TypeClassConstraint contravariant f, TypeClassConstraint applicative f ]
                            (Fn
                                (Fn (Var a) (App (Var f) (Var a)))
                                (Fn (Var s) (App (Var f) (Var s)))
                            )
                        )
        , test "Fold1" <|
            \() ->
                opticType fold1
                    |> Expect.equal
                        (Constrained [ TypeClassConstraint contravariant f, TypeClassConstraint apply f ]
                            (Fn
                                (Fn (Var a) (App (Var f) (Var a)))
                                (Fn (Var s) (App (Var f) (Var s)))
                            )
                        )
        ]
