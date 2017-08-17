module Library exposing (..)

{-| Type classes making up the part of the Haskell library which is referred to by the
lens types.

This will be the place to encode everything we need to know about the relationships
between types to infer what can be used where.

# Typically applied to `p`:
@docs profunctor, choice

# Typically applied to `f`:
@docs functor, applicative, apply, contravariant

# Monad and related classes, used by some of the functions over lenses:
@docs monad, monadReader, monadState, const, ident

# Related to Setting:
@docs settable, distributive, traversable, foldable

# Other
@docs isDefined
-}

import Haskell exposing (..)


{-| Profunctor.
-}
profunctor : TypeClass
profunctor =
    TypeClass "Profunctor" (Supers [])


{-| Choice.
-}
choice : TypeClass
choice =
    TypeClass "Choice" (Supers [ profunctor ])


{-| Indicates the existence of an instance for a certain type class applied to
a certain type(s).
-}
isDefined : TypeClass -> List Type -> Bool
isDefined tc ts =
    ts == [ Prefix FnOp ] && (tc == profunctor || tc == choice)


{-| Functor.
-}
functor : TypeClass
functor =
    TypeClass "Functor" (Supers [])


{-| Applicative.
-}
applicative : TypeClass
applicative =
    TypeClass "Applicative" (Supers [ functor ])


{-| Apply.
-}
apply : TypeClass
apply =
    TypeClass "Apply" (Supers [ functor ])


{-| Contravariant.
-}
contravariant : TypeClass
contravariant =
    TypeClass "Contravariant" (Supers [])


{-| Monad.
-}
monad : TypeClass
monad =
    TypeClass "Monad" (Supers [ applicative ])


{-| MonadReader.
-}
monadReader : TypeClass
monadReader =
    TypeClass "MonadReader" (Supers [ monad ])


{-| MonadState.
-}
monadState : TypeClass
monadState =
    TypeClass "MonadState" (Supers [ monad ])


{-| Const.
-}
const : TypeConstructor
const =
    TypeConstructor "Const"


{-| Identity. Note: name shortened to avoid colliding with the built in (`identity`)[Basic#identity] fn.
-}
ident : TypeConstructor
ident =
    TypeConstructor "Identity"


{-| Settable, which combines apparently all the things and is effectively provided only for
Identity (and things isomorphic to it).
-}
settable : TypeClass
settable =
    TypeClass "Settable" (Supers [ applicative, distributive, traversable ])


{-| Distributive, the dual of Traversable.
-}
distributive : TypeClass
distributive =
    TypeClass "Distributive" (Supers [ functor ])


{-| Traversable.
-}
traversable : TypeClass
traversable =
    TypeClass "Traversable" (Supers [ functor, foldable ])


{-| Foldable.
-}
foldable : TypeClass
foldable =
    TypeClass "Foldable" (Supers [])
