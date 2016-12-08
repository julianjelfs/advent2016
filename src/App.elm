module App exposing (..)

import Html exposing (Html, text, div, button)
import Html.Attributes exposing (type_, class)
import Html.Events exposing (onClick)
import Day8 exposing (..)
import Time


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
        [ div
            []
            [ button
                [ type_ "button"
                , onClick NextInstruction ]
                [ text "Next instruction" ] ]
        , div []
            [ text ("Next Instruction: " ++ instToString (List.head model.instructions)) ]
        , div []
           (List.range 0 5
                |> List.map (\y ->
                    div [ class "row" ]
                        (List.range 0 49
                            |> List.map (\x ->
                                let
                                    p = getPixel (x, y) model.grid
                                    cls =
                                        case p of
                                            Nothing -> "cell"
                                            Just (_, _, l) -> if l then "cell lit" else "cell"
                                in
                                    div [ class cls ]
                                        [ text ((toString x) ++ ":" ++ (toString y))]
                            ))
                    ))
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    --Sub.none
    Time.every (Time.second * 0.2) (\t -> NextInstruction)
