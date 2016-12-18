module Day13 exposing (..)

import Bitwise exposing (and, shiftRightBy)
import Set

input = 1350

positionValid visited (x, y) =
    positive (x, y)
        && (not (Set.member (x, y) visited))
        && available (x, y)

positive (x, y) =
    x >= 0 && y >= 0

available (x, y) =
    (((x * x + 3 * x + 2 * x * y + y + y * y) + input )
        |> numberOfBitsSet) % 2 == 0

--http://www.playingwithpointers.com/swar.html
--I have no clue how this works
numberOfBitsSet i =
    let
        a =
            i - ( i
                |> shiftRightBy 1
                |> and 0x55555555 )
        b =
            ( and a 0x33333333 ) + ( a |> shiftRightBy 2 |> and 0x33333333 )
    in
        ( ( b + ( shiftRightBy 4 b ) ) |> and 0x0F0F0F0F ) * 0x01010101 |> shiftRightBy 24

nearbyCells (x, y) =
    [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]

getPossiblePositions visited position =
     (nearbyCells position)
        |> List.filter (positionValid visited)

evaluatePositions depth positions visited =
    let
        (v, n) =
            List.foldl (\p (visited, nextLevel) ->
                let
                    v = Set.insert p visited
                in
                    (v, List.append nextLevel (getPossiblePositions v p))
            ) (visited, []) positions
    in
        case depth == 50 of
            True ->
                Set.size v
            False ->
                evaluatePositions (depth + 1) n v

solution () =
    evaluatePositions 0 [(1,1)] Set.empty
