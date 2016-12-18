module Day11 exposing (..)

import Array exposing (Array)
import Set exposing (Set)
import List.Extra exposing (subsequences)
import Debug exposing (log)

type alias Position =
    { floors: Array Floor
    , elevatorIndex: Int
    }

type alias Floor =
    { things: Set Thing
    }

type alias Thing = (String, String)

{-initialPosition =
    Position
        (Array.fromList
            [ Floor
                (Set.fromList [ ("M", "H")
                , ("M", "L")
                ])
            , Floor
                (Set.fromList [ ("G", "H")
                ])
            , Floor
                (Set.fromList [ ("G", "L")
                ])
            , Floor
                Set.empty
            ])
        0-}

elements =
    ["T","PL","S","PR","R"]
--    ["H","L"]

initialPosition =
    Position
        (Array.fromList
            [ Floor
                (Set.fromList [ ("G", "T")
                , ("M", "T")
                , ("G", "PL")
                , ("G", "S")
                ])
            , Floor
                (Set.fromList [ ("M", "PL")
                , ("M", "S")
                ])
            , Floor
                (Set.fromList [ ("G", "PR")
                , ("M", "PR")
                , ("G", "R")
                , ("M", "R")
                ])
            , Floor
                Set.empty
            ])
        0

pairs pos =
    elements
        |> List.concatMap (\e ->
            let
                floors t =
                    Array.indexedMap (\i f -> if Set.member (t, e) f.things then i else -1) pos.floors
                        |> Array.filter (\i -> i >= 0)
                        |> Array.toList

                chipsFloors =
                    floors "M"

                genFloors =
                    floors "G"
            in
                List.map2 (\c g -> (c,g)) chipsFloors genFloors )
                    |> List.sort
                    |> (\s -> (pos.elevatorIndex, s))

hasMatchingGenerators (_, e) generators =
    Set.member ("G", e) generators

floorValid floor =
    case Set.size floor.things of
        0 -> True
        1 -> True
        _ ->
            let
                (chips, generators) =
                    Set.partition
                        (\(t, e) -> case t of
                            "M" -> True
                            _ -> False ) floor.things
            in
                --floor is valid if we have both unaccompanied chips && some generators
                if Set.isEmpty generators then
                    True
                else
                    Set.foldl (\c agg ->
                       agg && (hasMatchingGenerators c generators)
                    ) True chips

positionNotVisited visited position =
    Set.member (pairs position) visited |> not

positionValid visited position =
    (positionNotVisited visited position)
        && (position.floors
            |> Array.toList
            |> List.filter floorValid
            |> List.length
            |> ((==) 4))

floorIsEmpty position index =
    case Array.get index position.floors of
        Just f -> Set.isEmpty f.things
        Nothing -> True

--returns true if we have got all the things to the fourth floor
complete position =
    floorIsEmpty position 0
        && floorIsEmpty position 1
        && floorIsEmpty position 2

modifyThingsInFloor index things setFn position =
    let
        mf = Array.get index position.floors
    in
        case mf of
            Just floor ->
                { position | floors =
                    Array.set
                        index
                        { floor | things = setFn floor.things things }
                        position.floors
                }
            Nothing -> position


applyMove from (f, to) things =
    let
        updated =
            modifyThingsInFloor f things Set.diff from
                |> modifyThingsInFloor to things Set.union
    in
        { updated | elevatorIndex = to }

--returns all valid positions that we can get into from the passed in position
getPossiblePositions visited position =
    let
        things =
            Array.get position.elevatorIndex position.floors
                |> Maybe.map .things
                |> Maybe.withDefault Set.empty
                |> Set.toList

        paths =
            case position.elevatorIndex of
                0 -> [(0,1)]
                1 -> [(1,0), (1,2)]
                2 -> [(2,1), (2,3)]
                3 -> [(3,2)]
                _ -> []
    in
        List.concatMap (\p ->
            things
                |> subsequences
                |> List.filter
                    (\s ->
                        case s of
                            [x] -> True
                            [x,y] -> True
                            _ -> False )
                |> List.map
                    (\s -> applyMove position p (Set.fromList s))
                |> List.filter
                    (\candidate -> positionValid visited candidate)
        ) paths

evaluationPositions depth positions visited =
    let
        (f, v, n) =
            List.foldl (\p (foundSolution, visited, nextLevel) ->
                --if completed on a previous iteration
                if foundSolution then
                    (foundSolution, visited, nextLevel)
                else
                    case complete p of
                        True -> (True, visited, nextLevel)
                        False ->
                            let
                                v = Set.insert (pairs p) visited
                            in
                                (foundSolution, v, List.append nextLevel (getPossiblePositions v p))
            ) (False, visited, []) positions
    in
        case f of
            True ->
                depth
            False ->
                evaluationPositions (depth + 1) n v

solution () =
    evaluationPositions 0 [initialPosition] Set.empty
