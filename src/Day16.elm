module Day16 exposing (..)

import String exposing (reverse)


input =
    "10111100110001111"


targetLength =
    35651584


swap =
    String.map
        (\c ->
            case c of
                '0' ->
                    '1'

                '1' ->
                    '0'

                _ ->
                    c
        )


dragonCurve str =
    str |> String.reverse |> swap |> (\b -> str ++ "0" ++ b)


processPairs result input =
    let
        tail =
            String.dropLeft 2 input

        head =
            String.left 2 input
    in
        case head of
            "00" ->
                processPairs (String.cons '1' result) tail

            "11" ->
                processPairs (String.cons '1' result) tail

            "01" ->
                processPairs (String.cons '0' result) tail

            "10" ->
                processPairs (String.cons '0' result) tail

            _ ->
                if (String.length result) % 2 == 0 then
                    processPairs "" (result |> String.reverse)
                else
                    result |> String.reverse


checksum =
    String.slice 0 targetLength
        >> (processPairs "")


getLongEnoughDragonCurve input =
    let
        dc =
            dragonCurve input
    in
        if (String.length dc) >= targetLength then
            dc
        else
            getLongEnoughDragonCurve dc


calculateChecksum () =
    getLongEnoughDragonCurve input
        |> checksum
