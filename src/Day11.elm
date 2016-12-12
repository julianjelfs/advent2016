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

initialPosition =
    Position
        (Array.fromList
            [ Floor
                (Set.fromList [ ("C", "H")
                , ("C", "L")
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

{-initialPosition =
    Position
        (Array.fromList
            [ Floor
                (Set.fromList [ ("G", "T")
                , ("C", "T")
                , ("G", "PL")
                , ("G", "S")
                ])
            , Floor
                (Set.fromList [ ("C", "PL")
                , ("C", "S")
                ])
            , Floor
                (Set.fromList [ ("G", "PR")
                , ("C", "PR")
                , ("G", "R")
                , ("C", "R")
                ])
            , Floor
                Set.empty
            ])
        0-}

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
                            "C" -> True
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

positionNotVisited: List Position -> Position -> Bool
positionNotVisited visited position =
    List.member position visited |> not

positionValid: List Position -> Position -> Bool
positionValid visited position =
    (log "not visited" (positionNotVisited visited position))
        && (position.floors
            |> Array.toList
            |> List.filter floorValid
            |> List.length
            |> (log "valid floors")
            |> ((==) 4))

floorIsEmpty: Position -> Int -> Bool
floorIsEmpty position index =
    case Array.get index position.floors of
        Just f -> Set.isEmpty f.things
        Nothing -> False

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
                |> (log "subs")
                |> List.map
                    (\s -> applyMove position p (Set.fromList s))
                |> (log "moved")
                |> List.filter
                    (\p -> positionValid visited p)
        ) paths

evaluatePosition: Int -> List Position -> Position -> Maybe Int
evaluatePosition moveCount visited position  =
    case complete position of
        True -> Just moveCount
        False ->
            let
                v = position :: visited
            in
                getPossiblePositions v position
                    |> (log "possible")
                    |> List.filterMap (evaluatePosition (moveCount + 1) v)
                    |> List.minimum

solution: () -> Int
solution () =
    evaluatePosition 0 [] initialPosition
        |> Maybe.withDefault 0
