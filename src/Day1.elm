module Day1 exposing (..)

import String exposing (dropLeft, left, split, toInt)
import List exposing (foldl, map, repeat)
import Tuple exposing (first, second)
import Set exposing (Set)

type alias State =
    { pos: (Int, Int)
    , direction: Int
    , visited: Set (Int, Int)
    , firstDup: Maybe (Int, Int)
    }

toInstruction str =
    ( left 1 str
    , str |> Result.withDefault 0 << toInt << (dropLeft 1))

newDirection turn dir =
    case turn of
        "R" ->
            if dir < 3 then dir + 1 else 0
        "L" ->
            if dir > 0 then dir - 1 else 3
        _ -> dir

applyInstruction (dir, n) state =
    repeat n 0
        |>
            foldl (\_ s ->
                case s.direction of
                    0 -> move s (0, 1)
                    1 -> move s (1, 0)
                    2 -> move s (0, -1)
                    3 -> move s (-1, 0)
                    _ -> s
            ) { state | direction = newDirection dir state.direction }

move state (x, y) =
    let
        pos = ((first state.pos) + x, (second state.pos) + y)
        dup =
            case state.firstDup of
                Just p -> Just p
                Nothing ->
                    if Set.member pos state.visited then (Just pos) else Nothing
    in
        { state | pos = pos
        , firstDup = dup
        , visited = Set.insert pos state.visited }

distance {pos, firstDup} =
    let
        (x, y) =
            case firstDup of
                Just p -> p
                Nothing -> pos
    in
        abs x + abs y

input =
    "R4, R3, R5, L3, L5, R2, L2, R5, L2, R5, R5, R5, R1, R3, L2, L2, L1, R5, L3, R1, L2, R1, L3, L5, L1, R3, L4, R2, R4, L3, L1, R4, L4, R3, L5, L3, R188, R4, L1, R48, L5, R4, R71, R3, L2, R188, L3, R2, L3, R3, L5, L1, R1, L2, L4, L2, R5, L3, R3, R3, R4, L3, L4, R5, L4, L4, R3, R4, L4, R1, L3, L1, L1, R4, R1, L4, R1, L1, L3, R2, L2, R2, L1, R5, R3, R4, L5, R2, R5, L5, R1, R2, L1, L3, R3, R1, R3, L4, R4, L4, L1, R1, L2, L2, L4, R1, L3, R4, L2, R3, L1, L5, R4, R5, R2, R5, R1, R5, R1, R3, L3, L2, L2, L5, R2, L2, R5, R5, L2, R3, L5, R5, L2, R4, R2, L1, R3, L5, R3, R2, R5, L1, R3, L2, R2, R1"
        |> split(", ")
        |> map toInstruction
        |> (foldl applyInstruction (State (0,0) 0 Set.empty Nothing))
        |> distance

