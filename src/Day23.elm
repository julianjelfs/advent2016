module Day23 exposing (..)

import Array

initialRegister =
    Array.fromList [7,0,0,0]

slotIndexFromSlot slot =
    case slot of
        "a" -> 0
        "b" -> 1
        "c" -> 2
        "d" -> 3
        _ -> -1

safeInt =
    String.toInt >> (Result.withDefault 0)

getSlotOrValue x reg =
    case slotIndexFromSlot x of
        -1 -> safeInt x
        n -> Array.get n reg |> Maybe.withDefault 0

parseInstruction inst =
    case String.words inst of
        cmd :: a :: b :: [] ->
            --this one deals with cpy and jnz
            (\(index, reg, toggles) ->
                let
                    toggle = Set.member index toggles
                in
                    case cmd of
                        "cpy" ->
                            let
                                from = getSlotOrValue a reg
                                to = slotIndexFromSlot b
                            in
                                (index + 1, Array.set to from reg, toggles)
                        _ ->
                            let
                                from = getSlotOrValue a reg
                                toIndex =
                                    if from > 0 then
                                        index + (safeInt b)
                                    else
                                        index + 1
                            in
                                (toIndex, reg, toggles))

        {--
            this is made a lot more awkward by the way I did day 12, but I don't want to start from scratch
        --}
        "tgl" :: slot :: [] ->
            (\(index, reg, toggles) ->
                ( index
                , reg
                , (Set.insert (getSlotOrValue slot reg) toggles)))

        cmd :: slot :: [] ->
            --this one deals with inc and dec
            (\(index, reg, toggles) ->
                let
                    toggle = Set.member index toggles

                    change =
                        case cmd of
                            "dec" -> (\v -> v - 1)
                            _ -> (\v -> v + 1)

                    slotIndex =
                        slotIndexFromSlot slot

                    updated =
                         Array.get slotIndex reg
                            |> Maybe.andThen (\v -> Just (change v))
                            |> Maybe.andThen (\v -> Just (Array.set slotIndex v reg))
                 in
                    case updated of
                        Nothing -> (index, reg, toggles)
                        Just r -> (index + 1, r, toggles))

        _ -> (\x -> x)

processInstruction (index, reg, toggles) inp =
    case Array.get index inp of
        Nothing -> reg
        Just fn ->
            processInstruction (fn (index, reg, toggles)) inp

solution () =
    testInput
        |> Array.map parseInstruction
        |> processInstruction (0, initialRegister, Set.empty)

testInput =
    Array.fromList
    [ "cpy 2 a"
    , "tgl a"
    , "tgl a"
    , "tgl a"
    , "cpy 1 a"
    , "dec a"
    , "dec a"
    ]

input =
    Array.fromList
    [ "cpy a b"
    , "dec b"
    , "cpy a d"
    , "cpy 0 a"
    , "cpy b c"
    , "inc a"
    , "dec c"
    , "jnz c -2"
    , "dec d"
    , "jnz d -5"
    , "dec b"
    , "cpy b c"
    , "cpy c d"
    , "dec d"
    , "inc c"
    , "jnz d -2"
    , "tgl c"
    , "cpy -16 c"
    , "jnz 1 c"
    , "cpy 99 c"
    , "jnz 77 d"
    , "inc a"
    , "inc d"
    , "jnz d -2"
    , "inc c"
    , "jnz c -5"
    ]
