module Day5 exposing (..)

import MD5 exposing (..)
import Array exposing (..)
import String exposing (toInt, length, fromList)

input = "reyedfim"

--call this with 0 "" to get the answer
crack n pw =
    let
        md5 = hex (input ++ (toString n))
    in
       case String.slice 0 5 md5 of
            "00000" ->
                let
                    p = pw ++ (String.slice 5 6 md5)
                in
                    case String.length p of
                        8 -> p
                        _ -> crack (n + 1) p
            _ -> crack (n + 1) pw

validPos s =
    case s of
        Ok n ->
            case (n >= 0 && n <= 7, n) of
                True -> Just n
                False -> Nothing
        Err _ -> Nothing

validCode slots n c =
    case get n slots of
        Nothing -> False
        Just s -> s == "_" || s /= c

slotsFilled slots =
    filter (\s -> s == "_") slots
        |> isEmpty

--call this with 0 "" (repeat 8 -1) to get the answer
crack2 n slots =
    let
        md5 = hex (input ++ (toString n))
        front = String.slice 0 5 md5
        pos = String.slice 5 6 md5 |> toInt
        code = String.slice 6 7 md5
    in
        case front == "00000" of
            False -> crack2 (n + 1) slots
            True ->
                case validPos pos of
                    Nothing -> crack2 (n + 1) slots
                    Just n ->
                        case validCode slots n code of
                            False -> crack2 (n + 1) slots
                            True ->
                                let
                                    updateSlots = set n code slots
                                in
                                    case slotsFilled updateSlots of
                                        True ->
                                            updateSlots
                                                |> toList
                                                |> (List.foldl (++) "")
                                        _ -> crack2 (n + 1) updateSlots

