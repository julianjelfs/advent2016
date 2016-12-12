module Day11 exposing (..)

type alias State =
    { position: Position
    , visited: List Position
    }

type alias Position =
    { floors: List Floor
    }

type alias Floor =
    { elevator: Bool
    , things: List Thing
    }

type Element =
    Thulium
    | Plutonium
    | Strontium
    | Promethium
    | Ruthenium

type Thing =
    Chip Element
    | Generator Element

initialState =
    { position =
        Position
            [ Floor True
                [ (Generator Thulium)
                , (Chip Thulium)
                , (Generator Plutonium)
                , (Generator Strontium)
                ]
            , Floor False
                [ (Chip Plutonium)
                , (Chip Strontium)
                ]
            , Floor False
                [ (Generator Promethium)
                , (Chip Promethium)
                , (Generator Ruthenium)
                , (Chip Ruthenium)
                ]
            , Floor False
                [ ]
            ]
    , visited = []
    }

thingsOfType things t =
    List.filter
        (\t -> case t of
            _ t -> True
            _ -> False) things

floorValid floor =
    let
        (chips, generators) =
            List.partition
                (\t -> case t of
                    Chip _ -> True
                    _ -> False ) floor.things
    in
        --floor is valid if we have both unaccompanied chips && some generators
        (not (List.empty generators))
            &&
                List.filter
                    (\c ->
                        thingsOfType generators c
                            |> List.isEmpty) chips
                |> List.isEmpty

positionValid pos =
    List.all floorValid pos.floors

getNextPossiblePositions state =
    --given the current position, get all possible next states
    --and then filter them for validity
    --and then filter them for states we've been in before
    []


solution () =
    getNextPossiblePositions initialState
        |> .visited
        |> length
