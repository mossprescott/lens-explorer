module Main exposing (main)

import Browser
import Haskell exposing (TypeVar, typeToSrc)
import Html exposing (Html, div, fieldset, input, label, table, td, text, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Lens.Functions exposing (fnToSrc, functions)
import Lens.Render exposing (..)
import Lens.Standard exposing (..)
import Lens.Types exposing (..)
import Type exposing (..)


main =
    Browser.sandbox
        { init = initialState
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { aligned : Bool
    , simple : Bool
    , regular : Bool
    , composed : List Optic
    }


initialState : Model
initialState =
    Model True False False [ orSame simplify lens, prism ]



-- UPDATE


type Msg
    = SetAligned Bool
    | SetSimple Bool
    | SetRegular Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetAligned newValue ->
            { model | aligned = newValue }

        SetSimple newValue ->
            { model | simple = newValue }

        SetRegular newValue ->
            { model | regular = newValue }



-- VIEW


view : Model -> Html Msg
view model =
    let
        prepareIf b f x =
            if b then
                f x

            else
                x

        prepareOptic =
            prepareIf model.simple (orSame simplify)

        prepareType =
            prepareIf (not model.regular) (orSame irregular)

        optics =
            List.map prepareOptic allOptics

        renderSpan o =
            Html.p [] [ nodeToHtml (opticToSrc prepareType o) ]

        renderRow o =
            tr [] (List.map (\n -> td [] [ nodeToHtml n ]) (opticToSrcRow prepareType o))
    in
    div [ style "margin" "20px" ]
        [ div [ style "margin-bottom" "20px" ]
            [ checkbox (SetAligned (not model.aligned)) "Aligned" model.aligned
            , checkbox (SetSimple (not model.simple)) "Simple" model.simple
            , checkbox (SetRegular (not model.regular)) "Regular" model.regular
            ]
        , if model.aligned then
            table [] (List.map renderRow optics)

          else
            div [] (List.map renderSpan optics)
        , div []
            [ Html.p [] [ text "l :: (a -> f a) -> c -> f c" ]
            , Html.p []
                [ nodeToHtml
                    (Words
                        ([ Name "l", Symbol "=" ]
                            ++ List.intersperse (Symbol ".")
                                (List.map
                                    (\l ->
                                        Words
                                            ([ Symbol "("
                                             , Name "_"
                                             , Symbol "::"
                                             , Name l.name
                                             ]
                                                ++ List.map (\v -> Name v.name) (opticParams l)
                                                ++ [ Symbol ")" ]
                                            )
                                    )
                                    model.composed
                                )
                        )
                    )
                ]
            ]
        , div [] (List.map (div [] << List.singleton << nodeToHtml << fnToSrc) functions)
        ]


checkbox : msg -> String -> Bool -> Html msg
checkbox msg name selected =
    label
        [ style "padding" "20px"
        ]
        [ input [ type_ "checkbox", checked selected, onClick msg ] []
        , text name
        ]
