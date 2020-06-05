module Lens.Types exposing (..)

{-| Types and basic operations on Optic, an ADT for representing lens types and
translating that to Haskell ASTs.
-}

import Haskell exposing (..)
import Library exposing (..)
import Type exposing (..)


{-| An ADT for `Lens` and all of the related types from the lens package.
-}
type alias Optic =
    { name : String
    , effect : OpticEffect
    , arrow : OpticArrow
    , forward : OpticSubjects
    , back : Maybe OpticSubjects
    }


{-| Encodes what type of effect the optic runs in. Most commonly, this is some
variable (`f`), with at least `Functor` required. In a few cases, it is a specific,
partially-applied type constructor.
-}
type OpticEffect
    = ConstrainedEffect TypeVar (List TypeClass)
    | FixedEffect TypeConstructor (List TypeVar)


{-| Encodes what type of "arrow" is applied to values. Most commonly, this is just
function application but it may be either a variable with constraints (e.g. `Profunctor`)
or a fixed parameter.
Note: I'm almost certainly abusing the term "arrow" here.
-}
type OpticArrow
    = FnArrow
    | ConstrainedArrow TypeVar (List TypeClass)
    | FixedArrow TypeVar


{-| Names of the "subject" and "value" types for the optic, often `s` and `a`,
or `t` and `b`.
-}
type alias OpticSubjects =
    { subject : TypeVar, value : TypeVar }


{-| Reduce an optic to its "Simple" or "primed" form, if possible, by replacing `t`s and `b`s with
`s`s and `a`s.
-}
simplify : Optic -> Maybe Optic
simplify o =
    case o.back of
        Just _ ->
            Just { o | name = o.name ++ "'", back = Nothing }

        Nothing ->
            Nothing


orSame : (a -> Maybe a) -> a -> a
orSame f x =
    Maybe.withDefault x (f x)


{-| Replace prefix `(->)` with infix `->` so that function and non-function types
look more familiar. Note that this runs on the _type_; it no longer makes sense to
rewrite the original Optic in this way.
TODO: this is currently pretty half-assed; it only looks at a couple of the nodes
that we know occur near the top.
-}
irregular : Type -> Maybe Type
irregular typ =
    case typ of
        App (App (Prefix FnOp) t1) t2 ->
            Just (Fn t1 t2)

        Fn t1 t2 ->
            Maybe.map2 Fn (irregular t1) (irregular t2)

        Constrained cs t ->
            Maybe.map (Constrained cs) (irregular t)

        _ ->
            Nothing
