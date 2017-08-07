module Lens exposing (..)

import Haskell exposing (..)
import Html exposing (Html)
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



-- Typeclasses applied to `p`:


profunctor =
    TypeClass "Profunctor" []


choice =
    TypeClass "Choice" [ profunctor ]



-- TypeClasses applied to `f`:


functor =
    TypeClass "Functor" []


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


simpleLens =
    Optic "Lens'"
        []
        [ functor ]
        (Fn (Var a) (App (Var f) (Var a)))
        (Fn (Var s) (App (Var f) (Var s)))


iso =
    Optic "Iso"
        [ profunctor ]
        [ functor ]
        (App2 (Var p) (Var a) (App (Var f) (Var b)))
        (App2 (Var p) (Var s) (App (Var f) (Var t)))


opticType : Optic -> Type
opticType o =
    Constrained (List.map (\c -> TypeClassConstraint c p) o.pClasses ++ List.map (\c -> TypeClassConstraint c f) o.fClasses) (Fn o.from o.to)



-- TODO: compute actual params


opticToSrc : Optic -> Html msg
opticToSrc o =
    words [ name o.name, name "s", name "t", name "a", name "b", symbol "::", keyword "forall", name "p", name "f", symbol ".", parenthesize 0 (typeToSrc (opticType o)) ]
