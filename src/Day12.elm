module Day12 exposing (..)

import Array

initialRegister =
    Array.fromList [0,0,0,0]

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
            (\(index, reg) ->
                case cmd of
                    "cpy" ->
                        let
                            from = getSlotOrValue a reg
                            to = slotIndexFromSlot b
                        in
                            (index + 1, Array.set to from reg)
                    _ ->
                        let
                            from = getSlotOrValue a reg
                            toIndex = if from > 0 then safeInt b else 0
                        in
                            (index + toIndex, reg))
        cmd :: slot :: [] ->
            --this one deals with inc and dec
            (\(index, reg) ->
                let
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
                        Nothing -> (index, reg)
                        Just r -> (index + 1, r))
        _ -> (\x -> x)

processInstruction (index, reg) inp =
    case Array.get index inp of
        Nothing -> reg
        Just fn ->
            processInstruction (fn (index, reg)) inp

solution () =
    input
        |> Array.map parseInstruction
        |> processInstruction (0, initialRegister)

input =
    Array.fromList
    [ "cpy 1 a"
    , "cpy 1 b"
    , "cpy 26 d"
    , "jnz c 2"
    , "jnz 1 5"
    , "cpy 7 c"
    , "inc d"
    , "dec c"
    , "jnz c -2"
    , "cpy a c"
    , "inc a"
    , "dec b"
    , "jnz b -2"
    , "cpy c b"
    , "dec d"
    , "jnz d -6"
    , "cpy 17 c"
    , "cpy 18 d"
    , "inc a"
    , "dec d"
    , "jnz d -2"
    , "dec c"
    , "jnz c -5"
    ]