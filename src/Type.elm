module Type
    exposing
        ( keyword
        , name
        , symbol
        , words
        , juxt
        )

import Html exposing (Html, p, span, text)
import Html.Attributes exposing (..)


sourceSans =
    ( "font-family", "Source Sans Pro, sans-serif" )


keyword : String -> Html msg
keyword str =
    span [ style [ sourceSans, ( "font-weight", "600" ) ] ] [ text str ]


name : String -> Html msg
name str =
    span [ style [ sourceSans, ( "font-style", "italic" ) ] ] [ text str ]


symbol : String -> Html msg
symbol str =
    span [ style [ sourceSans ] ] [ text str ]


words : List (Html msg) -> Html msg
words ns =
    span [] (List.intersperse (symbol " ") ns)


juxt : List (Html msg) -> Html msg
juxt ns =
    span [] ns
