module Haskell exposing (..)

{-| Haskell types and operations on them. This is not really a full representation of
Haskell types, but is enough to express the types of parametric functions and typeclass
constraints.

# Basics
@docs Type, TypeVar, Op, Constraint, TypeConstructor, TypeClass, app

# Aliases
@docs TypeAlias, aliasRef

# Utilities
@docs typeToSrc, constraintToSrc, prec, substitute
-}

import Type exposing (..)


{-| A named type variable.
-}
type TypeVar
    = TypeVar { name : String }


{-| A type class, including references to any type classes it inherits from.
-}
type TypeClass
    = TypeClass
        { name : String
        , supers : List TypeClass
        }


{-| A constraint on a type paramater.
-}
type Constraint
    = TypeClassConstraint TypeClass (List TypeVar)
    | Equivalent Type TypeVar


{-| A (type-level) operator; that is, an infix type constructor such as `->`.
-}
type Op
    = Op { symbol : String }


{-| A type constructor.
-}
type TypeConstructor
    = TypeConstructor { name : String }


{-| A Haskell type.
-}
type Type
    = Unit
    | Var TypeVar
    | Constr TypeConstructor
    | App Type Type
      --| Infix Type Op Type
    | Fn Type Type
    | Prefix Op
    | Constrained (List Constraint) Type


{-| Nested Apps applying a curried fn to multiple args.
-}
app : Type -> List Type -> Type
app f ts =
    case ts of
        [] ->
            f

        t :: more ->
            app (App f t) more


{-| A type alias is a sort of type-level macro that can be expanded at any time.
-}
type TypeAlias
    = TypeAlias
        { name : String
        , args : List TypeVar
        , rhs : Type
        }


{-| When an alias is referenced, it looks like a type constructor.
-}
aliasRef : TypeAlias -> Type
aliasRef (TypeAlias alias) =
    Constr (TypeConstructor { name = alias.name })


{-| Eliminate one or more type variables by substituting a type expression for each
occurence.
-}
substitute : List ( TypeVar, Type ) -> Type -> Type
substitute pairs t =
    let
        match =
            List.head (List.filter (\( v, _ ) -> Var v == t) pairs)
    in
        case ( match, t ) of
            ( Just ( _, u ), _ ) ->
                u

            ( Nothing, Unit ) ->
                t

            ( Nothing, Var _ ) ->
                t

            ( Nothing, Constr _ ) ->
                t

            ( Nothing, App t1 t2 ) ->
                App (substitute pairs t1) (substitute pairs t2)

            --( Nothing, Infix t1 op t2 ) ->
            --    Infix (substitute pairs t1) op (substitute pairs t2)
            ( Nothing, Fn t1 t2 ) ->
                Fn (substitute pairs t1) (substitute pairs t2)

            ( Nothing, Prefix op ) ->
                t

            ( Nothing, Constrained cs t1 ) ->
                Constrained cs (substitute pairs t1)


{-| Precedence table for Haskell expressions, used in [`typeToSrc`](#typeToSrc).
-}
prec : { atom : Int, app : Int, infix : Int, fn : Int, constrained : Int, equiv : Int }
prec =
    { atom = 5
    , app = 4
    , infix = 3
    , fn = 2
    , constrained = 1
    , equiv = 0
    }


{-| Convert a type to Haskell syntax, along with an indication of the precendence of the outermost
  expression.
-}
typeToSrc : Type -> ( Precedence, Node )
typeToSrc t =
    case t of
        Unit ->
            ( prec.atom, Symbol "()" )

        Var (TypeVar v) ->
            ( prec.atom, Name v.name )

        Constr (TypeConstructor c) ->
            ( prec.atom, Name c.name )

        App t1 t2 ->
            parenthesizeLeftAssoc prec.app
                Nothing
                (typeToSrc t1)
                (typeToSrc t2)

        {-
           Infix t1 (Op op) t2 ->
               parenthesize prec.infix
                   (Just (Symbol op.symbol))
                   [ typeToSrc t1, typeToSrc t2 ]
        -}
        Fn t1 t2 ->
            parenthesizeRightAssoc prec.fn
                (Just (Symbol "→"))
                (typeToSrc t1)
                (typeToSrc t2)

        Prefix (Op op) ->
            ( prec.atom, Juxt [ Symbol "(", Symbol op.symbol, Symbol ")" ] )

        Constrained cs t ->
            parenthesize prec.constrained
                (Just (Symbol "⇒"))
                ( prec.atom
                , Juxt ([ Symbol "(" ] ++ List.intersperse (Symbol ", ") (List.map constraintToSrc cs) ++ [ Symbol ")" ])
                )
                (typeToSrc t)


{-| Convert a constraint to Haskell syntax.
-}
constraintToSrc : Constraint -> Node
constraintToSrc c =
    case c of
        TypeClassConstraint (TypeClass tc) vs ->
            Words ([ Name tc.name ] ++ List.map (Name << (\(TypeVar v) -> v.name)) vs)

        Equivalent t v ->
            Tuple.second (parenthesize prec.infix (Just (Symbol "~")) (typeToSrc (Var v)) (typeToSrc t))
