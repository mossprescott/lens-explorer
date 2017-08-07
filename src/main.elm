module Main exposing (main)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
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
    }


model : Model
model =
    Model [ lens, simpleLens, iso ]



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    div [] (List.map renderOptic model.optics)


renderOptic o =
    div [] [ opticToSrc o ]



--div
--    [ style [ ( "font-family", "Source Sans Pro, sans-serif" ) ]
--    ]
--    [ text (opticToSrc o) ]
