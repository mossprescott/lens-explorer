module Main exposing (main)

import Html exposing (Html, div, fieldset, input, label, table, td, tr, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String
import Lens exposing (..)
import Type exposing (nodeToHtml)


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { optics : List Optic
    , aligned : Bool
    , simple : Bool
    }


model : Model
model =
    Model allOptics True False



-- UPDATE


type Msg
    = SetAligned Bool
    | SetSimple Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetAligned newValue ->
            { model | aligned = newValue }

        SetSimple newValue ->
            { model | simple = newValue }



-- VIEW


view : Model -> Html Msg
view model =
    let
        optics =
            if model.simple then
                (List.map simplified model.optics)
            else
                model.optics

        renderSpan o =
            Html.p [] [ nodeToHtml (opticToSrc o) ]

        renderRow o =
            tr [] (List.map (\n -> td [] [ nodeToHtml n ]) (opticToSrcRow o))
    in
        div []
            [ if model.aligned then
                table [] (List.map renderRow optics)
              else
                div [] (List.map renderSpan optics)
            , fieldset []
                [ checkbox (SetAligned (not model.aligned)) "Aligned" model.aligned
                , checkbox (SetSimple (not model.simple)) "Simple" model.simple
                ]
            ]


checkbox : msg -> String -> Bool -> Html msg
checkbox msg name selected =
    label
        [ style [ ( "padding", "20px" ) ]
        ]
        [ input [ type_ "checkbox", checked selected, onClick msg ] []
        , text name
        ]
