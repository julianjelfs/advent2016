module Day18 exposing (..)

import Array
import Tuple

input =
  ".^.^..^......^^^^^...^^^...^...^....^^.^...^.^^^^....^...^^.^^^...^^^^.^^.^.^^..^.^^^..^^^^^^.^^^..^"

--input =
--    ".^^.^.^^^^"

stringToArray =
    String.toList >> Array.fromList

safe p =
    p == "." || p == ""

trap p =
    p == "^"

ruleOne (l, c, r) =
    trap l && trap c && safe r

ruleTwo (l, c, r) =
    trap c && trap r && safe l

ruleThree (l, c, r) =
    trap l && safe r && safe c

ruleFour (l, c, r) =
    trap r && safe l && safe c

symbol prev =
    if ruleOne prev || ruleTwo prev || ruleThree prev || ruleFour prev then
        "^"
    else
        "."

getFromPrevious row index =
    ( String.slice (index-1) index row
    , String.slice index (index+1) row
    , String.slice (index+1) (index+2) row )

nextRow row =
    String.foldl
        (\c (i, next) ->
            let
                s =
                    getFromPrevious row i
                        |> symbol

                start =
                    String.slice 0 i next

                end =
                    String.slice (i+1) (String.length next) next
            in
                (i+1,
                start ++ s ++ end)
        ) (0, (String.repeat (String.length row) "."))
        row
        |> Tuple.second

safeCount =
    String.filter (\c -> c == '.') >> String.length

grid () =
    List.foldl
        (\i (rows, prev) ->
            let
                n = nextRow prev
            in
                (n :: rows, n)
        ) ([input], input)
        (List.range 0 (400000-2))
        |> Tuple.first
        |> (List.map safeCount)
        |> List.sum
