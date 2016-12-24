module Day21 exposing (..)

import Debug exposing (log)
import Regex exposing (..)

password = "abcdefgh"

stringToInt =
    String.toInt >> Result.withDefault 0

parsed =
    instructions
        |> String.lines
        |> List.map parseInstruction

parseInstruction str =
    case String.words str of
        "rotate" :: "based" :: _ :: _ :: _ :: _ :: l :: []
            -> rotateBasedOn l
        "rotate" :: "right" :: n :: _ :: []
            -> rotateRight (stringToInt n)
        "swap" :: "position" :: x :: _ :: _ :: y :: []
            -> swapPositions (stringToInt x) (stringToInt y)
        "swap" :: "letter" :: a :: _ :: _ :: b :: []
            -> swapLetters a b
        "move" :: _ :: x :: _ :: _ :: y :: []
            -> move (stringToInt x) (stringToInt y)
        "reverse" :: _ :: x :: _ :: y :: []
            -> reverse (stringToInt x) (stringToInt y)
        _ -> identity

scramble =
    parsed
        |> List.foldl (\i p -> i p) password

rotateLeft n str =
     (String.dropLeft n str) ++ (String.left n str)

rotateRight n str =
    (String.right n str) ++ (String.dropRight n str)

rotate dir n str =
    let
        n_ = n % (String.length str)
    in
        case dir of
            "left" -> rotateLeft n_ str
            "right" -> rotateRight n_ str
            _ -> str

indexOf c =
    String.indexes c >> List.head >> Maybe.withDefault -1

rotateBasedOn c str =
    let
        i =
            indexOf c str
        n =
            if i >= 4 then i + 2 else i + 1
    in
        if i < 0 then
            str
        else
            rotate "right" n str

swapLetters a b str =
    let
        ai =
            indexOf a str
        bi =
            indexOf b str
    in
        swapPositions ai bi str


stringToChar str =
    case str |> String.uncons of
        Just (c, _) -> c
        Nothing -> '_'

swapPositions x y str =
    let
        xv = String.slice x (x+1) str |> stringToChar
        yv = String.slice y (y+1) str |> stringToChar

    in
        String.map
            (\c ->
                if c == xv then
                    yv
                else
                    if c == yv then
                        xv
                    else
                        c
            ) str

move x y str =
    let
        xv = String.slice x (x+1) str
        withoutX = replace All (regex xv) (\_ -> "") str
    in
        (String.slice 0 y withoutX)
            ++ xv
            ++ (String.slice y (String.length str) withoutX)

reverse x y str =
    let
        rev =
            String.slice x (y+1) str
                |> String.reverse
    in
        (String.slice 0 x str)
            ++ rev
            ++ (String.slice (y+1) (String.length str) str)

{--
different kinds of instruction are:
    rotate left/right 2 steps
    rotate based on position of letter e
    swap letter h with letter f
    swap position 6 with position 4
    move position 1 to position 2
    reverse positions 6 through 7
--}

instructions = """rotate based on position of letter a
swap letter g with letter d
move position 1 to position 5
reverse positions 6 through 7
move position 5 to position 4
rotate based on position of letter b
reverse positions 6 through 7
swap letter h with letter f
swap letter e with letter c
reverse positions 0 through 7
swap position 6 with position 4
rotate based on position of letter e
move position 2 to position 7
swap position 6 with position 4
rotate based on position of letter e
reverse positions 2 through 3
rotate right 2 steps
swap position 7 with position 1
move position 1 to position 2
move position 4 to position 7
move position 5 to position 0
swap letter e with letter f
move position 4 to position 7
reverse positions 1 through 7
rotate based on position of letter g
move position 7 to position 4
rotate right 6 steps
rotate based on position of letter g
reverse positions 0 through 5
reverse positions 0 through 7
swap letter c with letter f
swap letter h with letter f
rotate right 7 steps
rotate based on position of letter g
rotate based on position of letter c
swap position 1 with position 4
move position 7 to position 3
reverse positions 2 through 6
move position 7 to position 0
move position 7 to position 1
move position 6 to position 7
rotate right 5 steps
reverse positions 0 through 6
move position 1 to position 4
rotate left 3 steps
swap letter d with letter c
move position 4 to position 5
rotate based on position of letter f
rotate right 1 step
move position 7 to position 6
swap position 6 with position 0
move position 6 to position 2
rotate right 1 step
swap position 1 with position 6
move position 2 to position 6
swap position 2 with position 1
reverse positions 1 through 7
move position 4 to position 1
move position 7 to position 0
swap position 6 with position 7
rotate left 1 step
reverse positions 0 through 4
rotate based on position of letter c
rotate based on position of letter b
move position 2 to position 1
rotate right 0 steps
swap letter b with letter d
swap letter f with letter c
swap letter d with letter a
swap position 7 with position 6
rotate right 0 steps
swap position 0 with position 3
swap position 2 with position 5
swap letter h with letter f
reverse positions 2 through 3
rotate based on position of letter c
rotate left 2 steps
move position 0 to position 5
swap position 2 with position 3
rotate right 1 step
rotate left 2 steps
move position 0 to position 4
rotate based on position of letter c
rotate based on position of letter g
swap position 3 with position 0
rotate right 3 steps
reverse positions 0 through 2
move position 1 to position 2
swap letter e with letter c
rotate right 7 steps
move position 0 to position 7
rotate left 2 steps
reverse positions 0 through 4
swap letter e with letter b
reverse positions 2 through 7
rotate right 5 steps
swap position 2 with position 4
swap letter d with letter g
reverse positions 3 through 4
reverse positions 4 through 5"""