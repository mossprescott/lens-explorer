module Haskell exposing (..)

import Html exposing (Html, span)
import Type exposing (..)


type TypeVar
    = TypeVar String


type TypeClass
    = TypeClass String (List TypeClass)


type Constraint
    = TypeClassConstraint TypeClass TypeVar
    | Equivalent Type TypeVar


type Op
    = Op String


type Type
    = Var TypeVar
    | App Type Type
    | App2 Type Type Type
    | Infix Type Op Type
    | Fn Type Type
    | Constrained (List Constraint) Type


parenthesize : Int -> ( Int, Html msg ) -> Html msg
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
        Var (TypeVar n) ->
            ( prec.atom, name n )

        App t1 t2 ->
            ( prec.app, words [ parenthesize prec.app (typeToSrc t1), symbol " ", parenthesize prec.app (typeToSrc t2) ] )

        -- TODO: make this smarter and use curried `App`s instead
        App2 t1 t2 t3 ->
            ( prec.app, words [ parenthesize prec.app (typeToSrc t1), symbol " ", parenthesize prec.app (typeToSrc t2), symbol " ", parenthesize prec.app (typeToSrc t3) ] )

        Infix t1 (Op op) t2 ->
            ( prec.infix, words [ parenthesize prec.infix (typeToSrc t1), symbol (" " ++ op ++ " "), parenthesize prec.infix (typeToSrc t2) ] )

        Fn t1 t2 ->
            ( prec.fn, words [ parenthesize prec.fn (typeToSrc t1), symbol " → ", parenthesize prec.fn (typeToSrc t2) ] )

        Constrained cs t ->
            ( prec.constrained, words [ symbol "(", juxt (List.intersperse (symbol ", ") (List.map constraintToSrc cs)), symbol ")", symbol "⇒", parenthesize prec.constrained (typeToSrc t) ] )


constraintToSrc : Constraint -> Html msg
constraintToSrc c =
    case c of
        TypeClassConstraint (TypeClass c _) (TypeVar v) ->
            words [ name c, name v ]

        Equivalent t (TypeVar v) ->
            words [ name v, symbol "~", parenthesize prec.equiv (typeToSrc t) ]
