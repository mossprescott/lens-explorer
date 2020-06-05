module LensTest exposing (composeTests, fancyTests, maybeReg, optic, sourceTests, typeTests)

import Expect exposing (..)
import Fuzz exposing (..)
import Haskell exposing (..)
import Lens.Compose exposing (..)
import Lens.Render exposing (..)
import Lens.Standard exposing (..)
import Lens.Types exposing (..)
import Library exposing (..)
import Test exposing (..)
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


optic =
    oneOf (List.map constant (allOptics ++ List.filterMap simplify allOptics))


maybeReg b =
    if b then
        identity

    else
        orSame irregular


fancyTests =
    describe "Fancy rendering matches type"
        [ {- Note: this test is pretty bogus right now because opticToSrc uses opticType
             directly. Keeping it in case that changes at some point.
          -}
          fuzz2 optic bool "src" <|
            \o reg ->
                let
                    src =
                        (nodeToString << opticToSrc (maybeReg reg)) o

                    typ =
                        (nodeToString << Tuple.second << typeToSrc << maybeReg reg << opticType) o
                in
                String.endsWith typ src
                    |> Expect.true "fancy src doesn't match type"
                    |> Expect.onFail (src ++ "\n  but the type looks like \n" ++ typ)
        , fuzz2 optic bool "srcRow" <|
            \o reg ->
                let
                    src =
                        (nodeToString << Type.Words << opticToSrcRow (maybeReg reg)) o

                    typ =
                        (nodeToString << Tuple.second << typeToSrc << maybeReg reg << opticType) o

                    stripWhitespace =
                        String.filter (\c -> c /= ' ')
                in
                String.endsWith (stripWhitespace typ) (stripWhitespace src)
                    |> Expect.true "fancy src doesn't match type"
                    |> Expect.onFail (src ++ "\n  but the type looks like \n" ++ typ)
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
        [ test "two simple lenses (TODO)" <|
            \() ->
                Maybe.map2 compose (simplify lens) (simplify lens)
                    |> Expect.equal
                        (Just
                            (Ok
                                -- Note: these all just substitutions on simpleLens
                                ( Optic "Lens'"
                                    (ConstrainedEffect f [ functor ])
                                    FnArrow
                                    (OpticSubjects x y)
                                    Nothing
                                , Optic "Lens'"
                                    (ConstrainedEffect f [ functor ])
                                    FnArrow
                                    (OpticSubjects y z)
                                    Nothing
                                , Optic "?"
                                    (ConstrainedEffect f [ functor ])
                                    FnArrow
                                    (OpticSubjects x z)
                                    Nothing
                                )
                            )
                        )
        ]
