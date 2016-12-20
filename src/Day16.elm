module Day16 exposing (..)

import String exposing (reverse)

input = "10111100110001111"

swap =
    String.toList
        >> List.map
            (\c ->
                case c of
                    '0' -> '1'
                    '1' -> '0'
                    _ -> c)
        >> String.fromList

dragonCurve str =
    str |> reverse |> swap |> (\b -> str ++ "0" ++ b)

calculateChecksum () =
   ""
