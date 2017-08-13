module Functions exposing (..)

import Haskell exposing (..)
import Lens exposing (s, t, a, b, applicative)
import Type exposing (Node(Name, Symbol, Words))


monad =
    TypeClass { name = "Monad", supers = [ applicative ] }


monadReader =
    TypeClass { name = "MonadReader", supers = [ monad ] }


monadState =
    TypeClass { name = "MonadState", supers = [ monad ] }


m =
    TypeVar { name = "m" }


r =
    TypeVar { name = "r" }


const =
    TypeConstructor { name = "Const" }


identity =
    TypeConstructor { name = "Identity" }


getting =
    TypeAlias
        { name = "Getting"
        , args = [ r, s, a ]
        , rhs =
            (Fn (Fn (Var a) (app (Constr const) [ (Var r), (Var a) ]))
                (Fn (Var s) (app (Constr const) [ (Var r), (Var s) ]))
            )
        }


aSetter =
    TypeAlias
        { name = "ASetter"
        , args = [ s, t, a, b ]
        , rhs =
            (Fn (Fn (Var a) (App (Constr identity) (Var b)))
                (Fn (Var s) (App (Constr identity) (Var t)))
            )
        }


functions : List ( String, Type )
functions =
    [ ( "view"
      , Constrained [ TypeClassConstraint monadReader [ m ] ]
            (Fn (app (aliasRef getting) [ (Var a), (Var s), (Var a) ]) (App (Var m) (Var a)))
      )
    , ( "set"
      , Fn (app (aliasRef aSetter) [ (Var s), (Var t), (Var a), (Var b) ]) (Fn (Var s) (Fn (Var s) (Var t)))
      )
    , ( "over"
      , Fn (app (aliasRef aSetter) [ (Var s), (Var t), (Var a), (Var b) ]) (Fn (Fn (Var a) (Var b)) (Fn (Var s) (Var t)))
      )
    , ( "assign"
      , Constrained [ TypeClassConstraint monadState [ s, m ] ]
            (Fn (app (aliasRef aSetter) [ (Var s), (Var t), (Var a), (Var b) ]) (Fn (Var b) (App (Var m) Unit)))
      )
    ]


fnToSrc : ( String, Type ) -> Node
fnToSrc ( name, typ ) =
    Words [ Name name, Symbol "::", Tuple.second (typeToSrc typ) ]
