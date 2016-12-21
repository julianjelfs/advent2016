module Day17 exposing (..)

import MD5 exposing (hex)
import Set

input = "gdjjyniy"

open =
    Set.fromList ['b', 'c', 'd', 'e', 'f']

getMove i (p, x, y) =
    case i of
        0 -> (p ++ "U", x, y-1)
        1 -> (p ++ "D", x, y+1)
        2 -> (p ++ "L", x-1, y)
        3 -> (p ++ "R", x+1, y)
        _ -> (p, x, y)

inbounds (p, x, y) =
    x >=0 && x < 4 && y >= 0 && y < 4

getNextPositions (p, x, y) =
    hex (input ++ p)
        |> String.left 4
        |> String.toList
        |> List.indexedMap
            (\i c ->
                if Set.member c open then
                    Just (getMove i (p, x, y))
                else
                    Nothing)
        |> List.filterMap identity
        |> List.filter inbounds

evaluatePositions positions =
    let
        (p, n) =
            List.foldl (\(p, x, y) (paths, nextLevel) ->
                case (x, y) of
                    (3, 3) -> (p :: paths, nextLevel)
                    _ ->
                        let
                            next =
                                getNextPositions (p, x, y)
                        in
                            (paths, List.append nextLevel next)
            ) ([], []) positions
    in
        case List.isEmpty n of
            True ->
                p
            False ->
                List.concat [p, (evaluatePositions n)]

solution () =
    evaluatePositions [("", 0,0)]
        |> List.map String.length
        |> List.maximum
