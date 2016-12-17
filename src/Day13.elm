module Day13 exposing (..)

input = 1350

{--
from current position find all possible next positions
for each one, recurse. Breadth first search until you find (31,39)
This is almost exactly the same as day 11
--}

getPossiblePositions visited position =
    {-- get all the surrounding positions
     filter out anything that has a negative element
     filter out anything that is a wall --}
    [position]

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

--one more thing to try which is to store visited pairs in a set

solution () =
    evaluationPositions 0 [(1,1)] Set.empty
