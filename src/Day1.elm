module Day1 exposing (..)

import String exposing (dropLeft, left, split, toInt)
import List exposing (map, foldl)
import Tuple exposing (first, second)

type alias State =
    { pos: (Int, Int)
    , direction: Int
    }

toInstruction str =
    ( left 1 str
    , str |> Result.withDefault 0 << toInt << (dropLeft 1))

newDirection turn dir =
    case turn of
        "R" ->
            dir + 1 % 4
        "L" ->
            dir - 1 % 4
        _ -> dir

applyInstruction (dir, n) state =
    let
        s =
            { state | direction = newDirection dir state.direction }
    in
        case s.direction of
            0 -> move s (0, n)
            1 -> move s (n, 0)
            2 -> move s (0, -n)
            3 -> move s (-n, 0)
            _ -> s

move ({pos} as state) (x, y) =
    { state | pos = ((first pos) + x, (second pos) + y) }

distance {pos} =
    let
        (x, y) = pos
    in
        abs x + abs y

input =
    "R4, R3, R5, L3, L5, R2, L2, R5, L2, R5, R5, R5, R1, R3, L2, L2, L1, R5, L3, R1, L2, R1, L3, L5, L1, R3, L4, R2, R4, L3, L1, R4, L4, R3, L5, L3, R188, R4, L1, R48, L5, R4, R71, R3, L2, R188, L3, R2, L3, R3, L5, L1, R1, L2, L4, L2, R5, L3, R3, R3, R4, L3, L4, R5, L4, L4, R3, R4, L4, R1, L3, L1, L1, R4, R1, L4, R1, L1, L3, R2, L2, R2, L1, R5, R3, R4, L5, R2, R5, L5, R1, R2, L1, L3, R3, R1, R3, L4, R4, L4, L1, R1, L2, L2, L4, R1, L3, R4, L2, R3, L1, L5, R4, R5, R2, R5, R1, R5, R1, R3, L3, L2, L2, L5, R2, L2, R5, R5, L2, R3, L5, R5, L2, R4, R2, L1, R3, L5, R3, R2, R5, L1, R3, L2, R2, R1"
        |> split(", ")
        |> map toInstruction
        |> (foldl applyInstruction (State (0,0) 0))
        |> distance

