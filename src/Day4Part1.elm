module Day4Part1 exposing (..)

import Day4Common exposing (input, parseRoom, validRoom)
import List exposing (filter, filterMap, map, sum)

getTheAnswer =
    filterMap parseRoom
        >> filter validRoom
        >> map .sectorId
        >> sum

partOneAnswer =
    getTheAnswer input

