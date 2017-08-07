module Lens exposing (..)

import Haskell exposing (..)
import Html exposing (Html, tr, td)
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



-- Type classes that are applied to `p`:


profunctor =
    TypeClass "Profunctor" []


choice =
    TypeClass "Choice" [ profunctor ]



-- Type classes that are applied to `f`:


functor =
    TypeClass "Functor" []


applicative =
    TypeClass "Applicative" [ functor ]


apply =
    TypeClass "Apply" [ functor ]


contravariant =
    TypeClass "Contravariant" []


type alias Optic =
    { name : String
    , pClasses : List TypeClass
    , fClasses : List TypeClass
    , from : Type
    , to : Type
    }


lens =
    Optic "Lens"
        []
        [ functor ]
        (Fn (Var a) (App (Var f) (Var b)))
        (Fn (Var s) (App (Var f) (Var t)))



--simpleLens =
--    Optic "Lens'"
--        []
--        [ functor ]
--        (Fn (Var a) (App (Var f) (Var a)))
--        (Fn (Var s) (App (Var f) (Var s)))


iso =
    Optic "Iso"
        [ profunctor ]
        [ functor ]
        (App2 (Var p) (Var a) (App (Var f) (Var b)))
        (App2 (Var p) (Var s) (App (Var f) (Var t)))


prism =
    Optic "Prism"
        [ choice ]
        [ applicative ]
        (App2 (Var p) (Var a) (App (Var f) (Var b)))
        (App2 (Var p) (Var s) (App (Var f) (Var t)))


traversal =
    Optic "Traversal"
        []
        [ applicative ]
        (Fn (Var a) (App (Var f) (Var b)))
        (Fn (Var s) (App (Var f) (Var t)))


fold =
    Optic "Fold"
        []
        [ contravariant, applicative ]
        (Fn (Var a) (App (Var f) (Var a)))
        (Fn (Var s) (App (Var f) (Var s)))


fold1 =
    Optic "Fold1"
        []
        [ contravariant, apply ]
        (Fn (Var a) (App (Var f) (Var a)))
        (Fn (Var s) (App (Var f) (Var s)))


allOptics =
    [ lens, iso, prism, traversal, fold, fold1 ]


opticType : Optic -> Type
opticType o =
    Constrained (List.map (\c -> TypeClassConstraint c p) o.pClasses ++ List.map (\c -> TypeClassConstraint c f) o.fClasses) (Fn o.from o.to)



-- TODO: compute actual params (dropping p, b, and t as needed)


opticToSrc : Optic -> Html msg
opticToSrc o =
    words [ name o.name, name "s", name "t", name "a", name "b", symbol "::", keyword "forall", name "p", name "f", symbol ".", parenthesize 0 (typeToSrc (opticType o)) ]


opticToSrcRow : Optic -> Html msg
opticToSrcRow o =
    tr []
        (List.map (\n -> td [] [ n ])
            [ name o.name
            , name "s"
            , name "t"
            , name "a"
            , name "b"
            , symbol "::"
            , keyword "forall"
            , name "p"
            , name "f"
            , symbol "."
            , symbol "("
              -- TODO: constraints
            , symbol ")"
            , symbol "⇒"
            , parenthesize prec.fn (typeToSrc (o.from))
            , symbol "→"
            , parenthesize 0 (typeToSrc (o.to))
              --, parenthesize 0 (typeToSrc (opticType o))
            ]
        )
