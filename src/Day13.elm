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
        |> setBits) % 2 == 0

-- http://www.playingwithpointers.com/swar.html
setBits i =
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
    let
        inc n = n + 1
        dec n = n - 1
        id n = n
    in
    [ ( dec, dec )
    , ( dec, id )
    , ( dec, inc )
    , ( id, dec )
    , ( id, inc )
    , ( inc, dec )
    , ( inc, id )
    , ( inc, inc )
    ] |> List.map
        (\(xfn, yfn) -> (xfn x, yfn y))

getPossiblePositions visited position =
     (nearbyCells position)
        |> List.filter (positionValid visited)

evaluationPositions depth positions visited =
    let
        (f, v, n) =
            List.foldl (\p (foundSolution, visited, nextLevel) ->
                if foundSolution then
                    (foundSolution, visited, nextLevel)
                else
                    case p == (31, 39) of
                        True -> (True, visited, nextLevel)
                        False ->
                            let
                                v = Set.insert p visited
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
    evaluationPositions 0 [(1,1)] Set.empty
