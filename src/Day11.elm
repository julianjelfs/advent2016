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

{--
initialPosition =
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
        0
--}

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

matchingGenerators: Set Thing -> Thing -> Set Thing
matchingGenerators generators (_, ce) =
    Set.filter
        (\(_, ge) -> ge == ce) generators

floorValid: Floor -> Bool
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
                (not (Set.isEmpty generators))
                    &&
                        (Set.filter
                            (\c ->
                                matchingGenerators generators c
                                    |> Set.isEmpty) chips
                            |> Set.isEmpty)

positionsEqual p1 p2 =
    p1.elevatorIndex == p2.elevatorIndex
        && (List.map2 (\f1 f2 -> f1.things == f2.things) (Array.toList p1.floors) (Array.toList p2.floors)
            |> List.all identity)


positionNotVisited: List Position -> Position -> Bool
positionNotVisited visited position =
    visited
        |> List.filter (positionsEqual position)
        |> List.isEmpty

positionValid: List Position -> Position -> Bool
positionValid visited position =
    (positionNotVisited visited position)
        && (position.floors
            |> Array.toList
            |> List.filter floorValid
            |> List.length
            |> ((==) 4))

floorIsEmpty: Position -> Int -> Bool
floorIsEmpty position index =
    case Array.get index position.floors of
        Just f -> Set.isEmpty f.things
        Nothing -> True

--returns true if we have got all the things to the fourth floor
complete: Position -> Bool
complete position =
    floorIsEmpty position 0
        && floorIsEmpty position 1
        && floorIsEmpty position 2

modifyThingsInFloor: Int -> Set Thing -> (Set Thing -> Set Thing -> Set Thing) -> Position -> Position
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


applyMove: Position -> (Int, Int) -> Set Thing -> Position
applyMove from (f, to) things =
    let
        updated =
            modifyThingsInFloor f things Set.diff from
                |> modifyThingsInFloor to things Set.union
    in
        { updated | elevatorIndex = to }

--returns all valid positions that we can get into from the passed in position
getPossiblePositions: List Position -> Position -> List Position
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

evaluatePosition: Int -> Int -> List Position -> Position -> Maybe Int
evaluatePosition shortest moveCount visited position  =
    if moveCount >= shortest then
        Nothing -- we already have a better solution
    else
        case complete position of
            True -> Just (log "completed" moveCount)
            False ->
                let
                    v = position :: visited
                    possible = getPossiblePositions v position

                in
                    Just (List.foldl (\poss shortest ->
                        case evaluatePosition shortest (moveCount + 1) v poss of
                            Nothing -> shortest
                            Just n ->
                                if n < shortest then
                                    n
                                else
                                    shortest
                    ) shortest possible)

solution: () -> Int
solution () =
    evaluatePosition 10000 0 [] initialPosition
        |> Maybe.withDefault 0
