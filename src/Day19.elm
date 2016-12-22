module Day19 exposing (..)

import Dict

numOfElves = 3014387

{--
Nicely explained here https://www.youtube.com/watch?v=uCsD3ZGzMgE
express n as 2 ^ a + l
then answer is 2 * l + 1
--}
partOne inp =
    floor (logBase 10 inp / logBase 10 2)
        |> toFloat
        |> ((^) 2.0)
        |> ((-) inp)
        |> ((*) 2)
        |> ((+) 1)

{--
    express n as 3 ^ a + l
--}
partTwo inp =
    let
        a = floor (logBase 10 inp / logBase 10 3)
            |> toFloat
            |> ((^) 3.0)
        b = inp - a
        c = floor (b / a)
    in
        if b == 0 then
            inp
        else
            ((c + 1) * (round b)) - (c * (round a))
                |> toFloat

