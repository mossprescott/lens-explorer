module Lens.Standard exposing (a, aSetter, allOptics, argumentOptics, b, extraOptics, f, fold, fold1, getter, getting, iso, lens, p, primaryOptics, prism, s, setter, setting, t, traversal)

{-| Define many of the standard types from the lens package as Optic values.
-}

import Haskell exposing (..)
import Lens.Types exposing (..)
import Library exposing (..)


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
        (FixedEffect const [ TypeVar "r" ])
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
