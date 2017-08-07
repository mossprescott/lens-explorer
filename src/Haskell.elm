module Haskell exposing (..)


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


parenthesize : Int -> ( Int, String ) -> String
parenthesize outer ( inner, str ) =
    if (inner <= outer) then
        "(" ++ str ++ ")"
    else
        str


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


typeToSrc : Type -> ( Precedence, String )
typeToSrc t =
    case t of
        Var (TypeVar n) ->
            ( prec.atom, n )

        App t1 t2 ->
            ( prec.app, parenthesize prec.app (typeToSrc t1) ++ " " ++ parenthesize prec.app (typeToSrc t2) )

        -- TODO: make this smarter and use curried `App`s instead
        App2 t1 t2 t3 ->
            ( prec.app, parenthesize prec.app (typeToSrc t1) ++ " " ++ parenthesize prec.app (typeToSrc t2) ++ " " ++ parenthesize prec.app (typeToSrc t3) )

        Infix t1 (Op op) t2 ->
            ( prec.infix, parenthesize prec.infix (typeToSrc t1) ++ " " ++ op ++ " " ++ parenthesize prec.infix (typeToSrc t2) )

        Fn t1 t2 ->
            ( prec.fn, parenthesize prec.fn (typeToSrc t1) ++ " → " ++ parenthesize prec.fn (typeToSrc t2) )

        Constrained cs t ->
            ( prec.constrained, "(" ++ String.join ", " (List.map constraintToSrc cs) ++ ") ⇒ " ++ parenthesize prec.constrained (typeToSrc t) )


constraintToSrc : Constraint -> String
constraintToSrc c =
    case c of
        TypeClassConstraint (TypeClass c _) (TypeVar v) ->
            c ++ " " ++ v

        Equivalent t (TypeVar v) ->
            v ++ " ~ " ++ parenthesize prec.equiv (typeToSrc t)
