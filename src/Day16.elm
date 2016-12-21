module Day16 exposing (..)

import String exposing (reverse)


input =
    "10111100110001111"


targetLength =
    35651584


swap =
    List.map
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
    str |> List.reverse |> swap |> (\b -> List.concat [ str, [ '0' ], b ])


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
        if (List.length dc) >= targetLength then
            dc |> List.take targetLength
            --|> String.fromList
        else
            getLongEnoughDragonCurve dc


calculateChecksum () =
    getLongEnoughDragonCurve (String.toList input)
        |> String.fromList
        |> checksum
