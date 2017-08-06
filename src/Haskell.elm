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
    | Infix Type Op Type
    | Fn Type Type
    | Constrained (List Constraint) Type


typeToSrc : Type -> String
typeToSrc t =
    case t of
        Var (TypeVar n) ->
            n

        App t1 t2 ->
            typeToSrc t1 ++ " " ++ typeToSrc t2

        Infix t1 (Op op) t2 ->
            typeToSrc t1 ++ " " ++ op ++ " " ++ typeToSrc t2

        Fn t1 t2 ->
            "(" ++ typeToSrc t1 ++ " -> " ++ typeToSrc t2 ++ ")"

        Constrained cs t ->
            "(" ++ String.join ", " (List.map constraintToSrc cs) ++ ") ⇒ " ++ typeToSrc t


constraintToSrc : Constraint -> String
constraintToSrc c =
    case c of
        TypeClassConstraint (TypeClass c _) (TypeVar v) ->
            c ++ " " ++ v

        Equivalent t (TypeVar v) ->
            v ++ " ~ " ++ typeToSrc t
