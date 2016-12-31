module Day25 exposing (..)

import Array
import Debug exposing (log)
import Set

initialRegister =
    Array.fromList [1,0,0,0]

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

out index reg slot inp =
    let
        s = log "S" (getSlotOrValue slot reg)
    in
        (index+1, reg, inp)

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
    --b should be a slot, but if it's been toggle from jnz it may be a number
    --in which case we skip
    case (String.toInt b) of
         Err _ ->
            let
                from = getSlotOrValue a reg
                to = slotIndexFromSlot b
            in
                (index + 1, Array.set to from reg, inp)
         Ok _ ->
            (index+1, reg, inp)

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
    | Out String

parseInstruction inst =
    case String.words inst of
        cmd :: a :: b :: [] ->
            case cmd of
                "cpy" -> Cpy a b
                _ -> Jnz a b
        cmd :: slot :: [] ->
            case cmd of
                "inc" -> Inc slot
                "dec" -> Dec slot
                _ -> Out slot
        _ -> Debug.crash ("unexpected instruction")

processInstruction iteration (index, reg, inp) =
    case Array.get index inp of
        Nothing -> reg
        Just i ->
            let
                res =
                    case i of
                        Inc slot -> inc index reg slot inp
                        Dec slot -> dec index reg slot inp
                        Out slot -> out index reg slot inp
                        Cpy a b -> cpy index reg a b inp
                        Jnz a b -> jnz index reg a b inp
            in
                case iteration > 1000 of
                    True -> reg
                    False -> processInstruction (iteration + 1) res

solution () =
    processInstruction 0 (0, initialRegister, (Array.map parseInstruction input))

input =
    String.lines raw
        |> Array.fromList

raw = """cpy a d
cpy 9 c
cpy 282 b
inc d
dec b
jnz b -2
dec c
jnz c -5
cpy d a
jnz 0 0
cpy a b
cpy 0 a
cpy 2 c
jnz b 2
jnz 1 6
dec b
dec c
jnz c -4
inc a
jnz 1 -7
cpy 2 b
jnz c 2
jnz 1 4
dec b
dec c
jnz 1 -4
jnz 0 0
out b
jnz a -19
jnz 1 -21"""