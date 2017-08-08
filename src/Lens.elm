module Lens exposing (..)

import Haskell exposing (..)
import Html exposing (Html, tr, td)
import Type exposing (..)


p =
    TypeVar { name = "p" }


f =
    TypeVar { name = "f" }


s =
    TypeVar { name = "s" }


t =
    TypeVar { name = "t" }


a =
    TypeVar { name = "a" }


b =
    TypeVar { name = "b" }


type alias Constrained =
    TypeVar -> Constraint



-- Type classes that are applied to `p`:


profunctor =
    TypeClass { name = "Profunctor", supers = [] }


choice =
    TypeClass { name = "Choice", supers = [ profunctor ] }



-- Type classes that are applied to `f`:


functor =
    TypeClass { name = "Functor", supers = [] }


applicative =
    TypeClass { name = "Applicative", supers = [ functor ] }


apply =
    TypeClass { name = "Apply", supers = [ functor ] }


contravariant =
    TypeClass { name = "Contravariant", supers = [] }


type alias Optic =
    { name : String
    , params : List TypeVar
    , pClasses : List TypeClass
    , fClasses : List TypeClass
    , from : Type
    , to : Type
    }


lens =
    Optic "Lens"
        [ s, t, a, b ]
        []
        [ functor ]
        (Fn (Var a) (App (Var f) (Var b)))
        (Fn (Var s) (App (Var f) (Var t)))


iso =
    Optic "Iso"
        [ s, t, a, b ]
        [ profunctor ]
        [ functor ]
        (App2 (Var p) (Var a) (App (Var f) (Var b)))
        (App2 (Var p) (Var s) (App (Var f) (Var t)))


prism =
    Optic "Prism"
        [ s, t, a, b ]
        [ choice ]
        [ applicative ]
        (App2 (Var p) (Var a) (App (Var f) (Var b)))
        (App2 (Var p) (Var s) (App (Var f) (Var t)))


traversal =
    Optic "Traversal"
        [ s, t, a, b ]
        []
        [ applicative ]
        (Fn (Var a) (App (Var f) (Var b)))
        (Fn (Var s) (App (Var f) (Var t)))


fold =
    Optic "Fold"
        [ s, a ]
        []
        [ contravariant, applicative ]
        (Fn (Var a) (App (Var f) (Var a)))
        (Fn (Var s) (App (Var f) (Var s)))


fold1 =
    Optic "Fold1"
        [ s, a ]
        []
        [ contravariant, apply ]
        (Fn (Var a) (App (Var f) (Var a)))
        (Fn (Var s) (App (Var f) (Var s)))


allOptics =
    [ lens, iso, prism, traversal, fold, fold1 ]


opticType : Optic -> Type
opticType o =
    Constrained (List.map (\c -> TypeClassConstraint c p) o.pClasses ++ List.map (\c -> TypeClassConstraint c f) o.fClasses) (Fn o.from o.to)


{-| Reduce an optic to its "Simple" or "primed" form, if possible, by replacing `t`s and `b`s with
`s`s and `a`s.
-}
simplify : Optic -> Maybe Optic
simplify o =
    let
        params =
            List.filter (\v -> v /= t && v /= b) o.params

        subs =
            substitute [ ( t, Var s ), ( b, Var a ) ]
    in
        if (params /= o.params) then
            Just
                { o
                    | name = o.name ++ "'"
                    , params = params
                    , from = subs o.from
                    , to = subs o.to
                }
        else
            Nothing


simplified : Optic -> Optic
simplified o =
    Maybe.withDefault o (simplify o)


opticToSrc : Optic -> Html msg
opticToSrc o =
    words ([ name o.name ] ++ (List.map (\(TypeVar v) -> name v.name) o.params) ++ [ symbol "::", keyword "forall", name "p", name "f", symbol ".", parenthesize 0 (typeToSrc (opticType o)) ])


classesToSrc : List TypeClass -> TypeVar -> Html msg
classesToSrc cs v =
    juxt (List.intersperse (symbol ", ") (List.map (\c -> constraintToSrc (TypeClassConstraint c v)) cs))


opticToSrcRow : Optic -> Html msg
opticToSrcRow o =
    tr []
        (List.map (\n -> td [] [ n ])
            [ name o.name
            , name "s"
            , if (List.member t o.params) then
                name "t"
              else
                symbol ""
            , name "a"
            , if (List.member b o.params) then
                name "b"
              else
                symbol ""
            , symbol "::"
            , keyword "forall"
            , name "p"
            , name "f"
            , symbol "."
            , symbol "("
            , classesToSrc o.pClasses p
            , symbol ","
            , classesToSrc o.fClasses f
            , symbol ")"
            , symbol "⇒"
            , parenthesize prec.fn (typeToSrc (o.from))
            , symbol "→"
            , parenthesize 0 (typeToSrc (o.to))
            ]
        )
