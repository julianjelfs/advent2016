module Day23 exposing (..)

import Array
import Debug exposing (log)
import Set

initialRegister =
    Array.fromList [12,0,0,0]

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

updateRegister slot value reg =
    let
        slotIndex =
            slotIndexFromSlot slot
    in
        Array.get slotIndex reg
            |> Maybe.andThen (\v -> Just value)
            |> Maybe.andThen (\v -> Just (Array.set slotIndex v reg))
            |> Maybe.withDefault reg

inc index reg slot inp =
    let
        slotIndex =
            slotIndexFromSlot slot

        updated =
             Array.get slotIndex reg
                |> Maybe.andThen (\v -> Just (v + 1))
                |> Maybe.andThen (\v -> Just (Array.set slotIndex v reg))
    in
        case updated of
            Nothing -> (index, reg, inp)
            Just r -> (index + 1, r, inp)

dec index reg slot inp =
    let
        slotIndex =
            slotIndexFromSlot slot

        updated =
             Array.get slotIndex reg
                |> Maybe.andThen (\v -> Just (v - 1))
                |> Maybe.andThen (\v -> Just (Array.set slotIndex v reg))
    in
        case updated of
            Nothing -> (index, reg, inp)
            Just r -> (index + 1, r, inp)

cpy index reg a b inp =
    let
        from = getSlotOrValue a reg
        to = slotIndexFromSlot b
    in
        (index + 1, Array.set to from reg, inp)

{--
    if a > 0 jump to index + b
--}
jnz index reg a b inp =
    let
        from = getSlotOrValue a reg
        toIndex =
            if from > 0 then
                index + (getSlotOrValue b reg)
            else
                index + 1
    in
        (toIndex, reg, inp)

type Instruction =
    Cpy String String
    | Jnz String String
    | Inc String
    | Dec String
    | Tgl String

parseInstruction inst =
    case String.words inst of
        cmd :: a :: b :: [] ->
            case cmd of
                "cpy" -> Cpy a b
                _ -> Jnz a b
        "tgl" :: slot :: [] ->
            Tgl slot
        cmd :: slot :: [] ->
            case cmd of
                "inc" -> Inc slot
                _ -> Dec slot
        _ -> Debug.crash ("unexpected instruction")

tgl input index reg slot =
    let
        i = index + (getSlotOrValue slot reg)

        maybeToggled =
            Array.get i input
                |> Maybe.andThen
                    (\inst ->
                        (case inst of
                            Inc slot -> Dec slot
                            Dec slot -> Inc slot
                            Tgl slot -> Inc slot
                            Cpy a b -> Jnz a b
                            Jnz a b -> Cpy a b)
                        |> Just)
    in
        case maybeToggled of
            Nothing -> (index+1, reg, input)
            Just toggled ->
                (index+1, reg, Array.set i toggled input)

getMultiplication index inp =
    if index + 2 >= (Array.length inp) then
        Nothing
    else
        let
            one = Array.get index inp
            two = Array.get (index+1) inp
            three = Array.get (index+2) inp
        in
            case one of
                Just (Inc a) ->
                    case two of
                        Just (Dec b) ->
                            case three of
                                Just (Jnz c d) ->
                                    if b == c && (safeInt d) == -2 then
                                        Just (a, b)
                                    else
                                        Nothing
                                _ -> Nothing
                        _ -> Nothing
                _ -> Nothing

processInstruction (index, reg, inp) =
    case Array.get index inp of
        Nothing -> reg
        Just i ->
            let
                res =
                    case i of
                        Inc slot ->
                            let
                                maybeMult =
                                    getMultiplication index inp
                            in
                                case maybeMult of
                                    Nothing ->
                                        inc index reg slot inp
                                    Just (a, b) ->
                                        let
                                            sum =
                                                (getSlotOrValue a reg) + (getSlotOrValue b reg)
                                        in
                                            ( index + 3
                                            , reg
                                                |> updateRegister a sum
                                                |> updateRegister b 0
                                            , inp )
                        Dec slot -> dec index reg slot inp
                        Cpy a b -> cpy index reg a b inp
                        Jnz a b -> jnz index reg a b inp
                        Tgl slot -> tgl inp index reg slot
            in
                processInstruction res

solution () =
    processInstruction (0, initialRegister, (Array.map parseInstruction input))

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
