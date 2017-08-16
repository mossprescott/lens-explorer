module Haskell exposing (..)

{-| Haskell types and operations on them. This is not really a full representation of
Haskell types, but is enough to express the types of parametric functions and typeclass
constraints.

# Basics
@docs Type, TypeVar, Op, Constraint, TypeConstructor, TypeClass, app

# Aliases
@docs TypeAlias, aliasRef

# Utilities
@docs typeToSrc, constraintToSrc, prec, Substitutions, substitute, composeFns
-}

import Dict
import Set exposing (Set)
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


{-| A mapping of type variables to be renamed.
Note: a list of pairs because Elm Dicts' keys can't have arbitrary types.
-}
type alias Substitutions =
    List ( TypeVar, TypeVar )


{-| Rewrite one or more type variables by substituting for each occurrence.
-}
substitute : Substitutions -> Type -> Type
substitute pairs t =
    let
        byName =
            Dict.fromList (List.map (Tuple.mapFirst (\(TypeVar v) -> v.name)) pairs)

        newVar (TypeVar v) =
            case (Dict.get v.name byName) of
                Just tv ->
                    tv

                Nothing ->
                    TypeVar v

        subConstr c =
            case c of
                TypeClassConstraint tc vs ->
                    TypeClassConstraint tc (List.map newVar vs)

                Equivalent _ _ ->
                    Debug.crash "unimplemented"
    in
        case t of
            Unit ->
                t

            Var v ->
                Var (newVar v)

            Constr _ ->
                t

            App t1 t2 ->
                App (substitute pairs t1) (substitute pairs t2)

            --Infix t1 op t2 ->
            --    Infix (substitute pairs t1) op (substitute pairs t2)
            Fn t1 t2 ->
                Fn (substitute pairs t1) (substitute pairs t2)

            Prefix op ->
                t

            Constrained cs t1 ->
                Constrained cs (substitute pairs t1)


{-| Construct a function type by composing two types in the manner of `.`, returning the
mappng of type names that was done on each side, and the resulting type:
Rename all type parameters so they're unique,
unify types and constraints, and finally try to
rename parameters, so that `*`s become `a`, `b`, etc. and `* -> *`s become `p` and `f`.
-}
composeFns : Type -> Type -> Result String ( Substitutions, Substitutions, Type )
composeFns left right =
    let
        vars : Type -> List TypeVar
        vars typ =
            let
                -- Note: can't put TypeVars in a Set(!)
                loop : Type -> Set String
                loop t =
                    case t of
                        Unit ->
                            Set.empty

                        Var (TypeVar v) ->
                            Set.singleton v.name

                        Constr _ ->
                            Set.empty

                        App t1 t2 ->
                            Set.union (loop t1) (loop t2)

                        Fn t1 t2 ->
                            Set.union (loop t1) (loop t2)

                        Prefix _ ->
                            Set.empty

                        Constrained cs t ->
                            Set.union (Set.fromList (List.concatMap constrVars cs)) (loop t)

                constrVars c =
                    case c of
                        TypeClassConstraint _ vs ->
                            List.map (\(TypeVar v) -> v.name) vs

                        Equivalent _ _ ->
                            Debug.crash "unimplemented"
            in
                List.map (\n -> TypeVar { name = n }) (Set.toList (loop typ))

        leftVars : Substitutions
        leftVars =
            List.indexedMap (\i v -> ( v, TypeVar { name = "l" ++ toString i } )) (vars left)

        rightVars : Substitutions
        rightVars =
            List.indexedMap (\i v -> ( v, TypeVar { name = "r" ++ toString (i + List.length leftVars) } )) (vars right)

        -- TODO
        result =
            Unit
    in
        Ok ( leftVars, rightVars, result )


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
