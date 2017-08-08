module Type exposing (..)

{-| This module provides a simple AST for Haskell type declarations and conversion to
HTML markup.
-}

import Html exposing (Html, p, span, text)
import Html.Attributes exposing (..)


{-| Simple AST for source code.
-}
type Node
    = Keyword String
    | Name String
    | Symbol String
    | Words (List Node)
    | Juxt (List Node)


type alias Precedence =
    Int


{-| Construct a Words node from a sequence of contained nodes, given the precedence of the
outer expression and each inner sub-expressions. Parens are inserted wherever the embedded
expression has lower (or equal) precedence.
-}
parenthesize : Precedence -> Maybe Node -> List ( Precedence, Node ) -> ( Precedence, Node )
parenthesize outer sepOpt pairs =
    let
        parens outer ( inner, n ) =
            if (inner <= outer) then
                Juxt [ Symbol "(", n, Symbol ")" ]
            else
                n

        pPairs =
            List.map (parens outer) pairs
    in
        ( outer
        , Words
            (case sepOpt of
                Nothing ->
                    pPairs

                Just sep ->
                    List.intersperse sep pPairs
            )
        )


{-| Translate AST nodes into styled HTML nodes.
-}
nodeToHtml : Node -> Html msg
nodeToHtml n =
    let
        sourceSans =
            ( "font-family", "Source Sans Pro, sans-serif" )
    in
        case n of
            Keyword str ->
                span [ style [ sourceSans, ( "font-weight", "600" ) ] ] [ text str ]

            Name str ->
                span [ style [ sourceSans, ( "font-style", "italic" ) ] ] [ text str ]

            Symbol str ->
                span [ style [ sourceSans ] ] [ text str ]

            Words ns ->
                nodeToHtml (Juxt (List.intersperse (Symbol " ") ns))

            Juxt ns ->
                span [] (List.map nodeToHtml ns)
