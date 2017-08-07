module Haskell exposing (..)

import Html exposing (Html, span)
import Type exposing (..)


type TypeVar
    = TypeVar { name : String }


type TypeClass
    = TypeClass
        { name : String
        , supers : List TypeClass
        }


type Constraint
    = TypeClassConstraint TypeClass TypeVar
    | Equivalent Type TypeVar


type Op
    = Op { symbol : String }


type Type
    = Var TypeVar
    | App Type Type
    | App2 Type Type Type
    | Infix Type Op Type
    | Fn Type Type
    | Constrained (List Constraint) Type


substitute : List ( TypeVar, Type ) -> Type -> Type
substitute pairs t =
    let
        match =
            List.head (List.filter (\( v, _ ) -> Var v == t) pairs)
    in
        case ( match, t ) of
            ( Just ( _, u ), _ ) ->
                u

            ( Nothing, Var _ ) ->
                t

            ( Nothing, App t1 t2 ) ->
                App (substitute pairs t1) (substitute pairs t2)

            ( Nothing, App2 t1 t2 t3 ) ->
                App2 (substitute pairs t1) (substitute pairs t2) (substitute pairs t3)

            ( Nothing, Infix t1 op t2 ) ->
                Infix (substitute pairs t1) op (substitute pairs t2)

            ( Nothing, Fn t1 t2 ) ->
                Fn (substitute pairs t1) (substitute pairs t2)

            ( Nothing, Constrained cs t1 ) ->
                Constrained cs (substitute pairs t1)


parenthesize : Precedence -> ( Precedence, Html msg ) -> Html msg
parenthesize outer ( inner, n ) =
    if (inner <= outer) then
        juxt [ symbol "(", n, symbol ")" ]
    else
        n


type alias Precedence =
    Int


prec =
    { atom = 5
    , app = 4
    , infix = 3
    , fn = 2
    , constrained = 1
    , equiv = 0
    }


typeToSrc : Type -> ( Precedence, Html msg )
typeToSrc t =
    case t of
        Var (TypeVar v) ->
            ( prec.atom, name v.name )

        App t1 t2 ->
            ( prec.app, words [ parenthesize prec.app (typeToSrc t1), parenthesize prec.app (typeToSrc t2) ] )

        -- TODO: make this smarter and use curried `App`s instead
        App2 t1 t2 t3 ->
            ( prec.app, words [ parenthesize prec.app (typeToSrc t1), parenthesize prec.app (typeToSrc t2), parenthesize prec.app (typeToSrc t3) ] )

        Infix t1 (Op op) t2 ->
            ( prec.infix, words [ parenthesize prec.infix (typeToSrc t1), symbol op.symbol, parenthesize prec.infix (typeToSrc t2) ] )

        Fn t1 t2 ->
            ( prec.fn, words [ parenthesize prec.fn (typeToSrc t1), symbol "→", parenthesize prec.fn (typeToSrc t2) ] )

        Constrained cs t ->
            ( prec.constrained, words [ symbol "(", juxt (List.intersperse (symbol ", ") (List.map constraintToSrc cs)), symbol ")", symbol "⇒", parenthesize prec.constrained (typeToSrc t) ] )


constraintToSrc : Constraint -> Html msg
constraintToSrc c =
    case c of
        TypeClassConstraint (TypeClass tc) (TypeVar v) ->
            words [ name tc.name, name v.name ]

        Equivalent t (TypeVar v) ->
            words [ name v.name, symbol "~", parenthesize prec.equiv (typeToSrc t) ]
