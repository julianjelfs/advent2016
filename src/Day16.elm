module Day16 exposing (..)

import String exposing (reverse)


input =
    "10111100110001111"


targetLength =
    35651584


swap =
    String.toList
        >> List.map
            (\c ->
                case c of
                    '0' ->
                        '1'

                    '1' ->
                        '0'

                    _ ->
                        c
            )
        >> String.fromList


dragonCurve str =
    str |> reverse |> swap |> (\b -> str ++ "0" ++ b)


processPairs result input =
    let
        tail =
            List.drop 2 input
    in
        case input of
            x :: y :: _ ->
                if x == y then
                    processPairs (result ++ "1") tail
                else
                    processPairs (result ++ "0") tail

            _ ->
                if (String.length result) % 2 == 0 then
                    processPairs "" (String.toList result)
                else
                    result


checksum =
    String.slice 0 targetLength
        >> String.toList
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
