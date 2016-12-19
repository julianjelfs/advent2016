module Day15 exposing (..)

import Array exposing (Array)

type alias Disc =
    { position: Int
    , slots: Int }

type alias State =
    { time: Int
    , discs: Array Disc }

numberOfDiscs = 2

--initialState =
--    State
--        0
--        Array.fromList
--            [ Disc 15 17
--            , Disc 2 3
--            , Disc 4 19
--            , Disc 2 13
--            , Disc 2 7
--            , Disc 0 5 ]

initialState =
    State
        0
        (Array.fromList
            [ Disc 4 5
            , Disc 1 2 ])

--this is not right, need to do a % thing
shiftPosition seconds disc =
    { disc | position = disc.position + seconds - disc.slots }

translate state seconds =
    { state | time = seconds
    , discs = (Array.map (shiftPosition seconds) state.discs) }


{--
    run the simulation with incrementing start times
    for each start time we then increment the time. With each
    tick we translate the state by + 1 and make sure that the disc
    at index t is at position 0. If not abandon, if so, next tick
--}

runSimulation startTime =
    let
        start = translate initialState startTime
    in
        List.foldl
            (\i (o, s) ->
                let
                    nextState = translate s 1
                    open =
                        case Array.get i s.discs of
                            Just d -> d.position == 0
                            Nothing -> False
                in
                    (o && open, nextState)
            ) (True, start) (List.range 0 (numberOfDiscs-1))
