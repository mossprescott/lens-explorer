module Lens.Render exposing (arrowClasses, constraintsToSrc, effectClasses, opticArrowType, opticEffectType, opticFnType, opticParams, opticToSrc, opticToSrcRow, opticType, opticUnivParams, opticVars)

{-| Functions translating the Optic ADT to Haskell types and the source code AST.
-}

import Haskell exposing (..)
import Lens.Types exposing (..)
import Type exposing (..)


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
arrowClasses : OpticArrow -> List Constraint
arrowClasses a =
    case a of
        FnArrow ->
            []

        ConstrainedArrow p tcs ->
            List.map (\tc -> TypeClassConstraint tc [ p ]) tcs

        FixedArrow _ ->
            []


{-| Type classes required for the "arrow" type.
-}
effectClasses : OpticEffect -> List Constraint
effectClasses e =
    case e of
        ConstrainedEffect f tcs ->
            List.map (\tc -> TypeClassConstraint tc [ f ]) tcs

        FixedEffect _ _ ->
            []


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
    app (opticArrowType o.arrow) [ Var a, App (opticEffectType o.effect) (Var b) ]


opticVars : Optic -> ( ( TypeVar, TypeVar ), ( TypeVar, TypeVar ) )
opticVars o =
    let
        ( s, a ) =
            ( o.forward.subject, o.forward.value )

        ( t, b ) =
            case o.back of
                Just ob ->
                    ( ob.subject, ob.value )

                Nothing ->
                    ( s, a )
    in
    ( ( s, t ), ( a, b ) )


opticType : Optic -> Type
opticType o =
    let
        ( ( s, t ), ( a, b ) ) =
            opticVars o
    in
    Constrained
        (arrowClasses o.arrow ++ effectClasses o.effect)
        (Fn (opticFnType o ( a, b )) (opticFnType o ( s, t )))


opticToSrc : (Type -> Type) -> Optic -> Node
opticToSrc prepare o =
    Words
        ([ Keyword "type", Name o.name ]
            ++ List.map (\v -> Name v.name) (opticParams o)
            ++ [ Symbol "=" ]
            ++ (if List.isEmpty (opticUnivParams o) then
                    []

                else
                    [ Keyword "forall" ] ++ List.map (\v -> Name v.name) (opticUnivParams o) ++ [ Symbol "." ]
               )
            ++ [ (Tuple.second << typeToSrc << prepare << opticType) o
               ]
        )


constraintsToSrc : List Constraint -> Node
constraintsToSrc cs =
    Juxt (List.intersperse (Symbol ", ") (List.map constraintToSrc cs))


{-| Convert to a list of Nodes, where the length of the list is always the same regardless of the
input. Therefore, when these rows are converted to a table, the corresponding nodes will always
appear in the same columns.
-}
opticToSrcRow : (Type -> Type) -> Optic -> List Node
opticToSrcRow prepare o =
    let
        ( ( s, t ), ( a, b ) ) =
            opticVars o
    in
    [ Keyword "type"
    , Name o.name
    , Words (List.map (\v -> Name v.name) (opticParams o))
    , Symbol "="
    , Words
        (if List.isEmpty (opticUnivParams o) then
            []

         else
            [ Keyword "forall" ] ++ List.map (\v -> Name v.name) (opticUnivParams o) ++ [ Symbol "." ]
        )
    , Symbol "("
    , let
        n =
            constraintsToSrc (arrowClasses o.arrow)
      in
      if List.isEmpty (arrowClasses o.arrow) || List.isEmpty (effectClasses o.effect) then
        n

      else
        Juxt [ n, Symbol "," ]
    , constraintsToSrc (effectClasses o.effect)
    , Symbol ")"
    , Symbol "⇒"

    -- Note: applying the "fn" precedence to wrap in parens only if it not a App:
    , parenthesizeOne prec.fn (typeToSrc (prepare (opticFnType o ( a, b ))))
    , Symbol "→"

    -- Note: never surrounding the "to" type with parens, which turns out to be the expected
    -- rendering, although it's mostly happenstance that it works out here.
    , Tuple.second (typeToSrc (prepare (opticFnType o ( s, t ))))
    ]
