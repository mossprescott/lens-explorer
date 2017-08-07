module Main exposing (main)

import Html exposing (Html, div, fieldset, input, label, table, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String
import Lens exposing (..)


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
    }


model : Model
model =
    Model allOptics True



-- UPDATE


type Msg
    = SetAligned Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetAligned newValue ->
            { model | aligned = newValue }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ if model.aligned then
            table [] (List.map opticToSrcRow model.optics)
          else
            div [] (List.map (\o -> Html.p [] [ opticToSrc o ]) model.optics)
        , fieldset [] [ checkbox (SetAligned (not model.aligned)) "Aligned" model.aligned ]
        ]


checkbox : msg -> String -> Bool -> Html msg
checkbox msg name selected =
    label
        [ style [ ( "padding", "20px" ) ]
        ]
        [ input [ type_ "checkbox", checked selected, onClick msg ] []
        , text name
        ]
