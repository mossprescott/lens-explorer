module Type
    exposing
        ( Node(..)
        , Precedence
        , parenthesize
        , parenthesizeLeftAssoc
        , parenthesizeRightAssoc
        , parenthesizeOne
        , nodeToHtml
        , nodeToString
        )

{-| This module provides a simple AST for Haskell type declarations and conversion to
HTML markup.

# AST
@docs Node, Precedence

# Rendering
@docs nodeToHtml, nodeToString

# Helpers
@docs parenthesize, parenthesizeLeftAssoc, parenthesizeRightAssoc, parenthesizeOne
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


{-| Precendence levels for parenthesization.
-}
type alias Precedence =
    Int


parenthesize_ :
    (Precedence -> Precedence -> Bool)
    -> (Precedence -> Precedence -> Bool)
    -> Precedence
    -> Maybe Node
    -> ( Precedence, Node )
    -> ( Precedence, Node )
    -> ( Precedence, Node )
parenthesize_ cmp1 cmp2 pOuter sepOpt p1 p2 =
    let
        p n =
            Juxt [ Symbol "(", n, Symbol ")" ]

        pPairs =
            [ parenthesizeOne_ cmp1 pOuter p1
            , parenthesizeOne_ cmp2 pOuter p2
            ]
    in
        ( pOuter
        , Words
            (case sepOpt of
                Nothing ->
                    pPairs

                Just sep ->
                    List.intersperse sep pPairs
            )
        )


parenthesizeOne_ : (Precedence -> Precedence -> Bool) -> Precedence -> ( Precedence, Node ) -> Node
parenthesizeOne_ cmp outer ( inner, n ) =
    if (cmp inner outer) then
        Juxt [ Symbol "(", n, Symbol ")" ]
    else
        n


{-| Wrap a node in parens if it has lower (or equal) precedence than the outer expression.
-}
parenthesizeOne : Precedence -> ( Precedence, Node ) -> Node
parenthesizeOne =
    parenthesizeOne_ (<=)


{-| Construct a Words node from a sequence of contained nodes, given the precedence of the
outer expression and each inner sub-expression. Parens are inserted wherever the embedded
expression has lower (or equal) precedence.
-}
parenthesize : Precedence -> Maybe Node -> ( Precedence, Node ) -> ( Precedence, Node ) -> ( Precedence, Node )
parenthesize =
    parenthesize_ (<=) (<=)


{-| Insert parens as need for a left-associative operator; that is, parens are not needed
around the left expression if it has the same precendence as the outer expression.
-}
parenthesizeLeftAssoc : Precedence -> Maybe Node -> ( Precedence, Node ) -> ( Precedence, Node ) -> ( Precedence, Node )
parenthesizeLeftAssoc =
    parenthesize_ (<) (<=)


{-| Insert parens as need for a right-associative operator; that is, parens are not needed
around the right expression if it has the same precendence as the outer expression.
-}
parenthesizeRightAssoc : Precedence -> Maybe Node -> ( Precedence, Node ) -> ( Precedence, Node ) -> ( Precedence, Node )
parenthesizeRightAssoc =
    parenthesize_ (<=) (<)


{-| Translate AST nodes into styled HTML nodes.
-}
nodeToHtml : Node -> Html msg
nodeToHtml n =
    let
        sourceSans =
            style "font-family" "Source Sans Pro, sans-serif"
    in
        case n of
            Keyword str ->
                span [ sourceSans,  style "font-weight" "600" ] [ text str ]

            Name str ->
                span [ sourceSans, style "font-style" "italic" ] [ text str ]

            Symbol str ->
                span [ sourceSans ] [ text str ]

            Words ns ->
                nodeToHtml (Juxt (List.intersperse (Symbol " ") ns))

            Juxt ns ->
                span [] (List.map nodeToHtml ns)


{-| Translate AST nodes into a String. Just flattens and strips the constructors.
-}
nodeToString : Node -> String
nodeToString n =
    case n of
        Keyword str ->
            str

        Name str ->
            str

        Symbol str ->
            str

        Words ns ->
            String.join " " (List.map nodeToString ns)

        Juxt ns ->
            String.join "" (List.map nodeToString ns)
