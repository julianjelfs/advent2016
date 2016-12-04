module Day4Part2 exposing (..)

import Char exposing (fromCode, toCode)
import Day4Common exposing (input, parseRoom, validRoom)
import List exposing (filter, filterMap, map)
import String exposing (fromList, toList)

shiftCode shift code =
    let
        n = shift + code
        n1 = n - 122
    in
        case code of
            45 -> 32
            _ ->
                if n1 > 0 then 96 + n1 else n

decrypt room =
    { room | name = (room.name
        |> toList
        |> map (toCode >> (shiftCode (room.sectorId % 26)) >> fromCode)
        |> fromList) }

getTheAnswer =
    filterMap parseRoom
        >> filter validRoom
        >> map decrypt
        >> filter (\r -> String.contains "northpole object storage" r.name)
        >> map .sectorId

partTwoAnswer =
    getTheAnswer input
