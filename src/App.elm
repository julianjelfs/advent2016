module App exposing (..)

import Html exposing (Html, text, div)
import Day1 exposing (input)

type alias Pixel = (Int, Int, Bool)

type alias Model =
    { grid = List Pixel
    }

init : ( Model, Cmd Msg )
init =
    ( { grid = [] }, Cmd.none )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div
        []
        []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
