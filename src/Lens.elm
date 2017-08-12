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
        (App (App (Var p) (Var a)) (App (Var f) (Var b)))
        (App (App (Var p) (Var s)) (App (Var f) (Var t)))


prism =
    Optic "Prism"
        [ s, t, a, b ]
        [ choice ]
        [ applicative ]
        (App (App (Var p) (Var a)) (App (Var f) (Var b)))
        (App (App (Var p) (Var s)) (App (Var f) (Var t)))


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


orSame : (a -> Maybe a) -> a -> a
orSame f x =
    Maybe.withDefault x (f x)


{-| Replace infix `->` with prefix `(->)` so that function and non-function types look more parallel.
-}
regular : Optic -> Maybe Optic
regular o =
    let
        fnToPrefix n =
            case n of
                Fn t1 t2 ->
                    Just (App (App (Prefix (Op { symbol = "→" })) t1) t2)

                _ ->
                    Nothing
    in
        Maybe.map2
            (\from to -> { o | from = from, to = to })
            (fnToPrefix o.from)
            (fnToPrefix o.to)


opticToSrc : Optic -> Node
opticToSrc o =
    Words
        ([ Name o.name ]
            ++ (List.map (\(TypeVar v) -> Name v.name) o.params)
            ++ [ Symbol "::", Keyword "forall", Name "p", Name "f", Symbol ".", Tuple.second (typeToSrc (opticType o)) ]
        )


classesToSrc : List TypeClass -> TypeVar -> Node
classesToSrc cs v =
    Juxt (List.intersperse (Symbol ", ") (List.map (\c -> constraintToSrc (TypeClassConstraint c v)) cs))


{-| Convert to a list of Nodes, where the length of the list is always the same regardless of the
input. Therefore, when these rows are converted to a table, the corresponding nodes will always
appear in the same columns.
-}
opticToSrcRow : Optic -> List Node
opticToSrcRow o =
    [ Name o.name
    , Name "s"
    , if (List.member t o.params) then
        Name "t"
      else
        Symbol ""
    , Name "a"
    , if (List.member b o.params) then
        Name "b"
      else
        Symbol ""
    , Symbol "::"
    , Keyword "forall"
    , Name "p"
    , Name "f"
    , Symbol "."
    , Symbol "("
    , let
        n =
            classesToSrc o.pClasses p
      in
        if ((List.isEmpty o.pClasses) || (List.isEmpty o.fClasses)) then
            n
        else
            Juxt [ n, Symbol "," ]
    , classesToSrc o.fClasses f
    , Symbol ")"
    , Symbol "⇒"
      -- Note: applying the "fn" precedence to wrap in parens only if it not a App:
    , Tuple.second (parenthesize prec.fn Nothing [ typeToSrc (o.from) ])
    , Symbol "→"
      -- Note: never surrounding the "to" type with parens, which turns out to be the expected
      -- rendering, although it's mostly happenstance that it works out here.
    , Tuple.second (typeToSrc (o.to))
    ]
