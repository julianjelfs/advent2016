module Day8 exposing (..)

import List exposing (head, range, map, concatMap, filter, foldl, length)
import String exposing (left, right, toInt, words)

getPixel: (Int, Int) -> List Pixel -> Maybe Pixel
getPixel (x, y) =
    filter (\(x1, y1, _) -> x1 == x && y1 == y)
        >> head

initialGrid: List Pixel
initialGrid =
    range 0 49
        |> concatMap (\x ->
            range 0 5 |> map (\y -> (x, y, False)))

type alias Pixel = (Int, Int, Bool)

type Dimension =
    Row Int Int
    | Col Int Int

type Instruction =
    Rect Int Int
    | Rotate Dimension
    | Unknown

safeToInt: String -> Int
safeToInt =
    toInt >> (Result.withDefault 0)

parseRotation: (Int -> Int -> Dimension) -> String -> String -> Dimension
parseRotation fn a b =
    case String.split "=" a of
        _ :: amount :: [] ->
            fn (amount |> safeToInt) (safeToInt b)
        _ -> Row 0 0

parseInstruction: String -> Instruction
parseInstruction inst =
    let
        w = words inst
    in
        case w of
            first :: second :: [] ->
                case String.split "x" second of
                    x :: y :: [] ->
                        Rect (x |> safeToInt) (y |> safeToInt)
                    _ -> Unknown
            _ :: dim :: axis :: _ :: amount :: [] ->
                case dim of
                    "row" ->
                        Rotate (parseRotation Row axis amount)
                    "column" ->
                        Rotate (parseRotation Col axis amount)
                    _ -> Unknown
            _ -> Unknown

lit: Pixel -> Bool
lit (_, _, l) =
    l

rectangle: Int -> Int -> List Pixel -> List Pixel
rectangle x y =
    map (\(x1, y1, l) ->
        if x1 < x && y1 < y then
            (x1, y1, True)
        else
            (x1, y1, l))

constrainedInc: Int -> Int -> Int -> Int
constrainedInc curr inc max =
    let
        res = curr + inc
    in
        if res > max then
            res - (max + 1)
        else
            res

incrementX: Pixel -> Int -> Pixel
incrementX (x, y, lit) n =
    (constrainedInc x n 49, y, lit)

incrementY: Pixel -> Int -> Pixel
incrementY (x, y, lit) n =
    (x, constrainedInc y n 5, lit)


rotate: Int -> Int -> (Int -> Int -> Int -> Bool) -> (Pixel -> Int -> Pixel) -> List Pixel -> List Pixel
rotate index n matcher incrementer =
    map (\(x, y, lit) ->
        if matcher x y index then
            incrementer (x, y, lit) n
        else
            (x, y, lit))

applyInstructions: Instruction -> List Pixel -> List Pixel
applyInstructions instr grid =
    case instr of
        Rect x y ->
            rectangle x y grid
        Rotate dim ->
            case dim of
                Row y n ->
                    rotate y n (\x y i -> y == i) incrementX grid
                Col x n ->
                    rotate x n (\x y i -> x == i) incrementY grid
        _ -> grid

instructions: List Instruction
instructions =
    input
        |> map parseInstruction

solution: Int
solution =
    input
        |> map parseInstruction
        |> foldl applyInstructions initialGrid
        |> filter lit
        |> length

input : List String
input =
    [ "rect 1x1"
    , "rotate row y=0 by 5"
    , "rect 1x1"
    , "rotate row y=0 by 6"
    , "rect 1x1"
    , "rotate row y=0 by 5"
    , "rect 1x1"
    , "rotate row y=0 by 2"
    , "rect 1x1"
    , "rotate row y=0 by 5"
    , "rect 2x1"
    , "rotate row y=0 by 2"
    , "rect 1x1"
    , "rotate row y=0 by 4"
    , "rect 1x1"
    , "rotate row y=0 by 3"
    , "rect 2x1"
    , "rotate row y=0 by 7"
    , "rect 3x1"
    , "rotate row y=0 by 3"
    , "rect 1x1"
    , "rotate row y=0 by 3"
    , "rect 1x2"
    , "rotate row y=1 by 13"
    , "rotate column x=0 by 1"
    , "rect 2x1"
    , "rotate row y=0 by 5"
    , "rotate column x=0 by 1"
    , "rect 3x1"
    , "rotate row y=0 by 18"
    , "rotate column x=13 by 1"
    , "rotate column x=7 by 2"
    , "rotate column x=2 by 3"
    , "rotate column x=0 by 1"
    , "rect 17x1"
    , "rotate row y=3 by 13"
    , "rotate row y=1 by 37"
    , "rotate row y=0 by 11"
    , "rotate column x=7 by 1"
    , "rotate column x=6 by 1"
    , "rotate column x=4 by 1"
    , "rotate column x=0 by 1"
    , "rect 10x1"
    , "rotate row y=2 by 37"
    , "rotate column x=19 by 2"
    , "rotate column x=9 by 2"
    , "rotate row y=3 by 5"
    , "rotate row y=2 by 1"
    , "rotate row y=1 by 4"
    , "rotate row y=0 by 4"
    , "rect 1x4"
    , "rotate column x=25 by 3"
    , "rotate row y=3 by 5"
    , "rotate row y=2 by 2"
    , "rotate row y=1 by 1"
    , "rotate row y=0 by 1"
    , "rect 1x5"
    , "rotate row y=2 by 10"
    , "rotate column x=39 by 1"
    , "rotate column x=35 by 1"
    , "rotate column x=29 by 1"
    , "rotate column x=19 by 1"
    , "rotate column x=7 by 2"
    , "rotate row y=4 by 22"
    , "rotate row y=3 by 5"
    , "rotate row y=1 by 21"
    , "rotate row y=0 by 10"
    , "rotate column x=2 by 2"
    , "rotate column x=0 by 2"
    , "rect 4x2"
    , "rotate column x=46 by 2"
    , "rotate column x=44 by 2"
    , "rotate column x=42 by 1"
    , "rotate column x=41 by 1"
    , "rotate column x=40 by 2"
    , "rotate column x=38 by 2"
    , "rotate column x=37 by 3"
    , "rotate column x=35 by 1"
    , "rotate column x=33 by 2"
    , "rotate column x=32 by 1"
    , "rotate column x=31 by 2"
    , "rotate column x=30 by 1"
    , "rotate column x=28 by 1"
    , "rotate column x=27 by 3"
    , "rotate column x=26 by 1"
    , "rotate column x=23 by 2"
    , "rotate column x=22 by 1"
    , "rotate column x=21 by 1"
    , "rotate column x=20 by 1"
    , "rotate column x=19 by 1"
    , "rotate column x=18 by 2"
    , "rotate column x=16 by 2"
    , "rotate column x=15 by 1"
    , "rotate column x=13 by 1"
    , "rotate column x=12 by 1"
    , "rotate column x=11 by 1"
    , "rotate column x=10 by 1"
    , "rotate column x=7 by 1"
    , "rotate column x=6 by 1"
    , "rotate column x=5 by 1"
    , "rotate column x=3 by 2"
    , "rotate column x=2 by 1"
    , "rotate column x=1 by 1"
    , "rotate column x=0 by 1"
    , "rect 49x1"
    , "rotate row y=2 by 34"
    , "rotate column x=44 by 1"
    , "rotate column x=40 by 2"
    , "rotate column x=39 by 1"
    , "rotate column x=35 by 4"
    , "rotate column x=34 by 1"
    , "rotate column x=30 by 4"
    , "rotate column x=29 by 1"
    , "rotate column x=24 by 1"
    , "rotate column x=15 by 4"
    , "rotate column x=14 by 1"
    , "rotate column x=13 by 3"
    , "rotate column x=10 by 4"
    , "rotate column x=9 by 1"
    , "rotate column x=5 by 4"
    , "rotate column x=4 by 3"
    , "rotate row y=5 by 20"
    , "rotate row y=4 by 20"
    , "rotate row y=3 by 48"
    , "rotate row y=2 by 20"
    , "rotate row y=1 by 41"
    , "rotate column x=47 by 5"
    , "rotate column x=46 by 5"
    , "rotate column x=45 by 4"
    , "rotate column x=43 by 5"
    , "rotate column x=41 by 5"
    , "rotate column x=33 by 1"
    , "rotate column x=32 by 3"
    , "rotate column x=23 by 5"
    , "rotate column x=22 by 1"
    , "rotate column x=21 by 2"
    , "rotate column x=18 by 2"
    , "rotate column x=17 by 3"
    , "rotate column x=16 by 2"
    , "rotate column x=13 by 5"
    , "rotate column x=12 by 5"
    , "rotate column x=11 by 5"
    , "rotate column x=3 by 5"
    , "rotate column x=2 by 5"
    , "rotate column x=1 by 5"
    ]
