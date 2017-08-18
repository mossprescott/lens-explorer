module Lens.Compose exposing (..)

{-| Operations to compose lens types, based on the Optic ADT.
-}

import Lens.Types exposing (..)


{-| (Attempt to) compose two optics, producing a version of each with renamed
parameters and one representing the composition.
-}
compose : Optic -> Optic -> Result String ( Optic, Optic, Optic )
compose left right =
    let
        commonArrow =
            case ( left.arrow, right.arrow ) of
                ( FnArrow, FnArrow ) ->
                    Ok FnArrow

                _ ->
                    Err ("TODO: Don't know how to combine: " ++ toString left.arrow ++ " and " ++ toString right.arrow)

        commonEffect =
            case ( left.effect, right.effect ) of
                ( ConstrainedEffect lf ltcs, ConstrainedEffect rf rtcs ) ->
                    Ok (ConstrainedEffect lf (commonTypeClasses ltcs rtcs))

                ( FixedEffect lconstr lvs, FixedEffect rconstr rvs ) ->
                    if (lconstr == rconstr) then
                        Ok (FixedEffect lconstr lvs)
                    else
                        Err ("Can't unify " ++ toString left.effect ++ " and " ++ toString right.effect)

                _ ->
                    Err ("TODO: Don't know how to combine: " ++ toString left.arrow ++ " and " ++ toString right.arrow)

        commonTypeClasses tcs1 tcs2 =
            -- TODO
            tcs1 ++ tcs2
    in
        Result.map2
            (\e a -> ( left, right, Optic "?" e a left.forward left.back ))
            commonEffect
            commonArrow


composeMany : List Optic -> ( List Optic, Optic )
composeMany optics =
    Debug.crash "not implemented"
