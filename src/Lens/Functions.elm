module Lens.Functions exposing (aSetter, fnToSrc, functions, getting, m, r)

import Haskell exposing (..)
import Lens.Standard exposing (a, b, s, t)
import Library exposing (..)
import Type exposing (Node(..))


m =
    TypeVar "m"


r =
    TypeVar "r"


getting =
    TypeAlias
        "Getting"
        [ r, s, a ]
        (Fn (Fn (Var a) (app (Constr const) [ Var r, Var a ]))
            (Fn (Var s) (app (Constr const) [ Var r, Var s ]))
        )


aSetter =
    TypeAlias
        "ASetter"
        [ s, t, a, b ]
        (Fn (Fn (Var a) (App (Constr ident) (Var b)))
            (Fn (Var s) (App (Constr ident) (Var t)))
        )


{-| Functions that take an optic and turn it into an operation on values. The type
of the resulting operation is typically simple, but which type it is depends on the
kind of optic you give it, which would be interesting to explore.

Note: all of these are left out:

  - functions that construct or transform optics, yielding an optic
  - functions involving MonadWriter
  - functions involving the "Indexed" optics
  - infix operators (many of which are aliases for these functions)

-}
functions : List ( String, Type )
functions =
    [ -- Getter:
      ( "view"
      , Constrained [ TypeClassConstraint monadReader [ s, m ] ]
            (Fn (app (aliasRef getting) [ Var a, Var s, Var a ]) (App (Var m) (Var a)))
      )

    -- views :: MonadReader s m => LensLike' (Const r) s a -> (a -> r) -> m r
    -- use :: MonadState s m => Getting a s a -> m a
    -- uses :: MonadState s m => LensLike' (Const r) s a -> (a -> r) -> m r
    -- Setter:
    , ( "set"
      , Fn (app (aliasRef aSetter) [ Var s, Var t, Var a, Var b ]) (Fn (Var s) (Fn (Var s) (Var t)))
      )

    -- set' :: ASetter' s a -> a -> s -> s
    , ( "over"
      , Fn (app (aliasRef aSetter) [ Var s, Var t, Var a, Var b ]) (Fn (Fn (Var a) (Var b)) (Fn (Var s) (Var t)))
      )
    , ( "assign"
      , Constrained [ TypeClassConstraint monadState [ s, m ] ]
            (Fn (app (aliasRef aSetter) [ Var s, Var t, Var a, Var b ]) (Fn (Var b) (App (Var m) Unit)))
      )
    , ( "modifying"
      , Constrained [ TypeClassConstraint monadState [ s, m ] ]
            (Fn (app (aliasRef aSetter) [ Var s, Var s, Var a, Var b ]) (Fn (Fn (Var a) (Var b)) (App (Var m) Unit)))
      )

    -- More
    -- Iso stuff:
    -- from :: AnIso s t a b -> Iso b a t s  -- an exception to the above?
    -- au :: Functor f => AnIso s t a b -> ((b -> t) -> f s) -> f a
    -- auf :: Optic (Costar f) g s t a b -> (f a -> g b) -> f s -> g t
    -- under :: AnIso s t a b -> (t -> s) -> b -> a
    -- Fold stuff:
    -- preview :: MonadReader s m => Getting (First a) s a -> m (Maybe a)
    -- previews :: MonadReader s m => Getting (First r) s a -> (a -> r) -> m (Maybe r)
    -- has :: Getting Any s a -> s -> Bool
    -- hasn't :: Getting All s a -> s -> Bool
    -- Xof: (X == Xof folded):
    -- foldMapOf :: Getting r s a -> (a -> r) -> s -> r
    -- foldOf :: Getting a s a -> s -> a
    -- toListOf :: Getting (Endo [a]) s a -> s -> [a]
    -- anyOf :: Getting Any s a -> (a -> Bool) -> s -> Bool
    -- allOf :: Getting All s a -> (a -> Bool) -> s -> Bool
    -- noneOf :: Getting Any s a -> (a -> Bool) -> s -> Bool
    -- andOf :: Getting All s Bool -> s -> Bool
    -- orOf :: Getting Any s Bool -> s -> Bool
    -- .. many more, but some have interesting types
    -- Review stuff:
    -- review :: MonadReader b m => AReview t b -> m t
    -- reviews :: MonadReader b m => AReview t b -> (t -> r) -> m r
    -- reuse :: MonadState b m => AReview t b -> m t
    -- reuses :: MonadState b m => AReview t b -> (t -> r) -> m r
    -- Traversal:
    -- traverseOf :: LensLike f s t a b -> (a -> f b) -> s -> f t
    -- sequenceAOf :: LensLike f s t (f b) b -> s -> f t
    -- Prism:
    -- is :: APrism s t a b -> s -> Bool
    -- Each:
    -- TODO: where does this fit in?
    -- each :: (Each s t a b) => Traversal s t a b
    ]


fnToSrc : ( String, Type ) -> Node
fnToSrc ( name, typ ) =
    Words [ Name name, Symbol "::", Tuple.second (typeToSrc typ) ]
