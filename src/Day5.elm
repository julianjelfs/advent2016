module Day5 exposing (..)

import MD5 exposing (..)
import Array exposing (..)
import String exposing (toInt, length, fromList)
import Debug exposing (log)

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

slotEmpty slots n =
    case get n slots of
        Nothing -> False
        Just s -> s == "_"

validPos slots md5 =
    case String.slice 5 6 md5 |> toInt of
        Ok n ->
            if n >= 0 && n <= 7 && (slotEmpty slots n) then
                Just n
            else
                Nothing
        Err _ -> Nothing

slotsFilled slots =
    filter (\s -> s == "_") slots
        |> isEmpty

--call this with 0 "" (repeat 8 "_") to get the answer
crack2 n slots =
    let
        md5 = hex (input ++ (toString n))
    in
        if String.slice 0 5 md5 == "00000" then
            case validPos slots md5 of
                Nothing -> crack2 (n+1) slots
                Just i ->
                    let
                        updated = set i (String.slice 6 7 md5) slots
                    in
                        if slotsFilled updated then
                            updated
                                |> toList
                                |> (List.foldr (++) "")
                        else
                            crack2 (n+1) updated
        else
            crack2 (n+1) slots


