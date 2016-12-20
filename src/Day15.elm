module Day15 exposing (..)

import Array exposing (Array)

initialState =
    Array.fromList
        [ (15, 17)
        , (2, 3)
        , (4, 19)
        , (2, 13)
        , (2, 7)
        , (0, 5)
        , (0, 11)]

shiftPosition seconds (position, slots) =
    ((position + seconds) % slots, slots)

translate state seconds =
    (Array.map (shiftPosition seconds) state)

runSimulation startTime =
    let
        (success, _) =
            List.foldl
                (\i (o, s) ->
                    if o then
                        let
                            nextState = translate s 1
                            open =
                                case Array.get i nextState of
                                    Just (position, _) -> position == 0
                                    Nothing -> False
                        in
                            (o && open, nextState)
                    else
                        (o, s)
                ) (True, translate initialState startTime)
                (List.range 0 ((Array.length initialState)-1))
    in
        if success then
            startTime
        else
            runSimulation (startTime + 1)

solution () =
    runSimulation 0
