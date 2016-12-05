module Day5 exposing (..)

import MD5 exposing (..)

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
