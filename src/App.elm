module App exposing (..)

import Html exposing (Html, text, div, button)
import Html.Attributes exposing (type_, class)
import Html.Events exposing (onClick)
import Day8 exposing (..)
import Day11 exposing (..)


type alias Model =
    { grid : List Pixel
    , instructions : List Instruction
    }

init : ( Model, Cmd Msg )
init =
    ( { grid = initialGrid
     , instructions = instructions }, Cmd.none )


type Msg
    = NextInstruction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextInstruction ->
            let
                head = (Maybe.withDefault Unknown (List.head model.instructions))
                tail = (Maybe.withDefault [] (List.tail model.instructions))
            in
                ( { model | instructions = tail
                , grid = applyInstructions head model.grid }, Cmd.none)

instToString: Maybe Instruction -> String
instToString m =
    case m of
        Nothing -> ""
        Just i -> toString i

view : Model -> Html Msg
view model =
    div
        []
        [ text (toString (Day11.solution ()))
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
   --Time.every (Time.second * 0.2) (\t -> NextInstruction)
