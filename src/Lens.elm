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


type alias Optic =
    { name : String
    , effect : OpticEffect
    , arrow : OpticArrow
    , forward : OpticSubjects
    , back : Maybe OpticSubjects
    }


{-| Ordinary type parameters, e.g. `s, t, a, b`.
-}
opticParams : Optic -> List TypeVar
opticParams o =
    let
        arrowPs =
            case o.arrow of
                FnArrow ->
                    []

                ConstrainedArrow _ _ ->
                    []

                FixedArrow v ->
                    [ v ]

        effectPs =
            case o.effect of
                ConstrainedEffect _ _ ->
                    []

                FixedEffect _ vs ->
                    vs

        subjectPs =
            case o.back of
                Just b ->
                    [ o.forward.subject, b.subject, o.forward.value, b.value ]

                Nothing ->
                    [ o.forward.subject, o.forward.value ]
    in
        arrowPs ++ effectPs ++ subjectPs


{-| Universally-quantified type parameters, e.g. `p, f`.
-}
opticUnivParams : Optic -> List TypeVar
opticUnivParams o =
    let
        arrowPs =
            case o.arrow of
                FnArrow ->
                    []

                ConstrainedArrow v _ ->
                    [ v ]

                FixedArrow _ ->
                    []

        effectPs =
            case o.effect of
                ConstrainedEffect v _ ->
                    [ v ]

                FixedEffect _ _ ->
                    []
    in
        arrowPs ++ effectPs


{-| Type classes required for the "arrow" type.
-}
arrowClasses : OpticArrow -> List TypeClass
arrowClasses a =
    case a of
        FnArrow ->
            []

        ConstrainedArrow _ tcs ->
            tcs

        FixedArrow _ ->
            []


{-| Type classes required for the "arrow" type.
-}
effectClasses : OpticEffect -> List TypeClass
effectClasses e =
    case e of
        ConstrainedEffect _ tcs ->
            tcs

        FixedEffect _ _ ->
            []


lens =
    Optic "Lens"
        (ConstrainedEffect f [ functor ])
        FnArrow
        (OpticSubjects s a)
        (Just (OpticSubjects t b))


iso =
    Optic "Iso"
        (ConstrainedEffect f [ functor ])
        (ConstrainedArrow p [ profunctor ])
        (OpticSubjects s a)
        (Just (OpticSubjects t b))


prism =
    Optic "Prism"
        (ConstrainedEffect f [ applicative ])
        (ConstrainedArrow p [ choice ])
        (OpticSubjects s a)
        (Just (OpticSubjects t b))


traversal =
    Optic "Traversal"
        (ConstrainedEffect f [ applicative ])
        FnArrow
        (OpticSubjects s a)
        (Just (OpticSubjects t b))


fold =
    Optic "Fold"
        (ConstrainedEffect f [ contravariant, applicative ])
        FnArrow
        (OpticSubjects s a)
        Nothing


fold1 =
    Optic "Fold1"
        (ConstrainedEffect f [ contravariant, apply ])
        FnArrow
        (OpticSubjects s a)
        Nothing


getter =
    Optic "Getter"
        (ConstrainedEffect f [ contravariant, functor ])
        FnArrow
        (OpticSubjects s a)
        Nothing


setter =
    Optic "Setter"
        (ConstrainedEffect f [ settable ])
        FnArrow
        (OpticSubjects s a)
        (Just (OpticSubjects t b))


{-| Getting is used in type signatures to indicate any optic that can produce values may be supplied.
-}
getting =
    Optic "Getting"
        (FixedEffect const [ (TypeVar "r") ])
        FnArrow
        (OpticSubjects s a)
        Nothing


setting =
    Optic "Setting"
        (FixedEffect ident [])
        (FixedArrow (TypeVar "p"))
        (OpticSubjects s a)
        (Just (OpticSubjects t b))


aSetter =
    Optic "ASetter"
        (FixedEffect ident [])
        FnArrow
        (OpticSubjects s a)
        (Just (OpticSubjects t b))


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


{-| (Unapplied) type of the effect, e.g. `f` or `Identity`.
-}
opticEffectType : OpticEffect -> Type
opticEffectType e =
    case e of
        ConstrainedEffect v _ ->
            Var v

        FixedEffect constr vs ->
            app (Constr constr) (List.map Var vs)


{-| (Unapplied) type of the arrow, e.g. `p` or `(->)`.
-}
opticArrowType : OpticArrow -> Type
opticArrowType a =
    case a of
        FnArrow ->
            Prefix FnOp

        ConstrainedArrow v _ ->
            Var v

        FixedArrow v ->
            Var v


{-| Type for one half of the optic.
-}
opticFnType : Optic -> ( TypeVar, TypeVar ) -> Type
opticFnType o ( a, b ) =
    app (opticArrowType o.arrow) [ (Var a), App (opticEffectType o.effect) (Var b) ]


opticVars : Optic -> ( TypeVar, TypeVar, TypeVar, TypeVar )
opticVars o =
    let
        ( s, a ) =
            ( o.forward.subject, o.forward.value )

        ( t, b ) =
            case o.back of
                Just b ->
                    ( b.subject, b.value )

                Nothing ->
                    ( s, a )
    in
        ( s, t, a, b )


opticType : Optic -> Type
opticType o =
    let
        ( s, t, a, b ) =
            opticVars o
    in
        Constrained
            -- TODO extract p and f from the instance
            (List.map (\c -> TypeClassConstraint c [ p ]) (arrowClasses o.arrow)
                ++ List.map (\c -> TypeClassConstraint c [ f ]) (effectClasses o.effect)
            )
            (Fn (opticFnType o ( a, b )) (opticFnType o ( s, t )))


opticToSrc : (Type -> Type) -> Optic -> Node
opticToSrc prepare o =
    Words
        ([ Keyword "type", Name o.name ]
            ++ (List.map (\v -> Name v.name) (opticParams o))
            ++ [ Symbol "=" ]
            ++ (if (List.isEmpty (opticUnivParams o)) then
                    []
                else
                    [ Keyword "forall" ] ++ (List.map (\v -> Name v.name) (opticUnivParams o)) ++ [ Symbol "." ]
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
    let
        ( s, t, a, b ) =
            opticVars o
    in
        [ Keyword "type"
        , Name o.name
        , Words (List.map (\v -> Name v.name) (opticParams o))
        , Symbol "="
        , Words
            (if (List.isEmpty (opticUnivParams o)) then
                []
             else
                [ Keyword "forall" ] ++ (List.map (\v -> Name v.name) (opticUnivParams o)) ++ [ Symbol "." ]
            )
        , Symbol "("
        , let
            n =
                -- TODO: extract p (and f) from the instance
                classesToSrc (arrowClasses o.arrow) p
          in
            if ((List.isEmpty (arrowClasses o.arrow)) || (List.isEmpty (effectClasses o.effect))) then
                n
            else
                Juxt [ n, Symbol "," ]
        , classesToSrc (effectClasses o.effect) f
        , Symbol ")"
        , Symbol "⇒"
          -- Note: applying the "fn" precedence to wrap in parens only if it not a App:
        , parenthesizeOne prec.fn (typeToSrc (prepare (opticFnType o ( a, b ))))
        , Symbol "→"
          -- Note: never surrounding the "to" type with parens, which turns out to be the expected
          -- rendering, although it's mostly happenstance that it works out here.
        , Tuple.second (typeToSrc (prepare (opticFnType o ( s, t ))))
        ]
