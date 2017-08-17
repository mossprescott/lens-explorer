module Lens exposing (..)

import Haskell exposing (..)
import Library exposing (..)
import Type exposing (..)


p =
    TypeVar "p"


f =
    TypeVar "f"


s =
    TypeVar "s"


t =
    TypeVar "t"


a =
    TypeVar "a"


b =
    TypeVar "b"


type alias Constrained =
    TypeVar -> Constraint


type alias FnBased =
    { fClasses : List TypeClass
    , f : TypeVar
    , s : TypeVar
    , a : TypeVar
    }


type alias VarBased =
    { pClasses : List TypeClass
    , fClasses : List TypeClass
    , s : TypeVar
    , a : TypeVar
    }


type Optic_
    = FnOptic FnBased
    | VarOptic VarBased



-- p: either (->) or p (and needs pClasses)
-- simple: either s and a or also t and b
-- typevars


type alias Optic =
    { name : String
    , params : List TypeVar
    , univ : List TypeVar
    , pClasses : List TypeClass
    , fClasses : List TypeClass
    , from : Type
    , to : Type
    }


lens =
    Optic "Lens"
        [ s, t, a, b ]
        [ f ]
        []
        [ functor ]
        (app (Prefix FnOp) [ (Var a), (App (Var f) (Var b)) ])
        (app (Prefix FnOp) [ (Var s), (App (Var f) (Var t)) ])


iso =
    Optic "Iso"
        [ s, t, a, b ]
        [ p, f ]
        [ profunctor ]
        [ functor ]
        (app (Var p) [ (Var a), (App (Var f) (Var b)) ])
        (app (Var p) [ (Var s), (App (Var f) (Var t)) ])


prism =
    Optic "Prism"
        [ s, t, a, b ]
        [ p, f ]
        [ choice ]
        [ applicative ]
        (app (Var p) [ (Var a), (App (Var f) (Var b)) ])
        (app (Var p) [ (Var s), (App (Var f) (Var t)) ])


traversal =
    Optic "Traversal"
        [ s, t, a, b ]
        [ f ]
        []
        [ applicative ]
        (app (Prefix FnOp) [ (Var a), (App (Var f) (Var b)) ])
        (app (Prefix FnOp) [ (Var s), (App (Var f) (Var t)) ])


fold =
    Optic "Fold"
        [ s, a ]
        [ f ]
        []
        [ contravariant, applicative ]
        (app (Prefix FnOp) [ (Var a), (App (Var f) (Var a)) ])
        (app (Prefix FnOp) [ (Var s), (App (Var f) (Var s)) ])


fold1 =
    Optic "Fold1"
        [ s, a ]
        [ f ]
        []
        [ contravariant, apply ]
        (app (Prefix FnOp) [ (Var a), (App (Var f) (Var a)) ])
        (app (Prefix FnOp) [ (Var s), (App (Var f) (Var s)) ])


getter =
    Optic "Getter"
        [ s, a ]
        [ f ]
        []
        [ contravariant, functor ]
        (app (Prefix FnOp) [ (Var a), (App (Var f) (Var a)) ])
        (app (Prefix FnOp) [ (Var s), (App (Var f) (Var s)) ])


setter =
    Optic "Setter"
        [ s, t, a, b ]
        [ f ]
        []
        [ settable ]
        (app (Prefix FnOp) [ (Var a), (App (Var f) (Var b)) ])
        (app (Prefix FnOp) [ (Var s), (App (Var f) (Var t)) ])


{-| Getting is used in type signatures to indicate any optic that can produce values may be supplied.
-}
getting =
    Optic "Getting"
        -- Note: unexpected param here
        [ TypeVar "r", s, a ]
        []
        []
        []
        (app (Prefix FnOp) [ (Var a), (app (Constr const) [ (Var (TypeVar "r")), (Var a) ]) ])
        (app (Prefix FnOp) [ (Var s), (app (Constr const) [ (Var (TypeVar "r")), (Var s) ]) ])


setting =
    Optic "Setting"
        -- Note: unexpected param here
        [ p, s, t, a, b ]
        []
        []
        []
        (app (Var p) [ (Var a), (App (Constr ident) (Var a)) ])
        (app (Var p) [ (Var s), (App (Constr ident) (Var s)) ])


aSetter =
    Optic "ASetter"
        [ s, t, a, b ]
        []
        []
        []
        (app (Prefix FnOp) [ (Var a), (App (Constr ident) (Var b)) ])
        (app (Prefix FnOp) [ (Var s), (App (Constr ident) (Var t)) ])


{-| The most commonly used types.
-}
primaryOptics =
    [ lens, iso, prism, traversal ]


{-| More limited types that are used less often.
-}
extraOptics =
    [ getter, setter, fold, fold1 ]


{-| Types that are typically only used in type signatures.
-}
argumentOptics =
    [ getting, setting, aSetter ]


allOptics =
    primaryOptics ++ extraOptics ++ argumentOptics


opticType : Optic -> Type
opticType o =
    Constrained (List.map (\c -> TypeClassConstraint c [ p ]) o.pClasses ++ List.map (\c -> TypeClassConstraint c [ f ]) o.fClasses) (Fn o.from o.to)


{-| (Attempt to) compose two optics, producing a version of each with renamed
parameters and one representing the composition.
-}
compose : Optic -> Optic -> ( Optic, Optic, Optic )
compose left right =
    Debug.crash "not implemented"


composeMany : List Optic -> ( List Optic, Optic )
composeMany optics =
    Debug.crash "not implemented"


{-| Reduce an optic to its "Simple" or "primed" form, if possible, by replacing `t`s and `b`s with
`s`s and `a`s.
-}
simplify : Optic -> Maybe Optic
simplify o =
    let
        params =
            List.filter (\v -> v /= t && v /= b) o.params

        subs =
            substitute [ ( t, s ), ( b, a ) ]
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


{-| Replace prefix `(->)` with infix `->` so that function and non-function types look more familiar.
TODO: this is currently pretty half-assed; it only looks at a couple of the nodes that we know occur
near the top.
-}
irregular : Type -> Maybe Type
irregular t =
    case t of
        App (App (Prefix FnOp) t1) t2 ->
            Just (Fn t1 t2)

        Fn t1 t2 ->
            Maybe.map2 Fn (irregular t1) (irregular t2)

        Constrained cs t ->
            Maybe.map (Constrained cs) (irregular t)

        _ ->
            Nothing


opticToSrc : (Type -> Type) -> Optic -> Node
opticToSrc prepare o =
    Words
        ([ Keyword "type", Name o.name ]
            ++ (List.map (\v -> Name v.name) o.params)
            ++ [ Symbol "=" ]
            ++ (if (List.isEmpty o.univ) then
                    []
                else
                    [ Keyword "forall" ] ++ (List.map (\v -> Name v.name) o.univ) ++ [ Symbol "." ]
               )
            ++ [ (Tuple.second << typeToSrc << prepare << opticType) o
               ]
        )


classesToSrc : List TypeClass -> TypeVar -> Node
classesToSrc cs v =
    Juxt (List.intersperse (Symbol ", ") (List.map (\c -> constraintToSrc (TypeClassConstraint c [ v ])) cs))


{-| Convert to a list of Nodes, where the length of the list is always the same regardless of the
input. Therefore, when these rows are converted to a table, the corresponding nodes will always
appear in the same columns.
-}
opticToSrcRow : (Type -> Type) -> Optic -> List Node
opticToSrcRow prepare o =
    [ Keyword "type"
    , Name o.name
    , Words (List.map (\v -> Name v.name) o.params)
    , Symbol "="
    , Words
        (if (List.isEmpty o.univ) then
            []
         else
            [ Keyword "forall" ] ++ (List.map (\v -> Name v.name) o.univ) ++ [ Symbol "." ]
        )
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
    , parenthesizeOne prec.fn (typeToSrc (prepare o.from))
    , Symbol "→"
      -- Note: never surrounding the "to" type with parens, which turns out to be the expected
      -- rendering, although it's mostly happenstance that it works out here.
    , Tuple.second (typeToSrc (prepare o.to))
    ]
