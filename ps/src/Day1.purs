module Day1 where
   
import Prelude
import Data.List (List, foldl)
import Data.Tuple
import Data.String (Pattern(..), split, uncons, take, drop)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)

input =
    "R4, R3, R5, L3, L5, R2, L2, R5, L2, R5, R5, R5, R1, R3, L2, L2, L1, R5, L3, R1, L2, R1, L3, L5, L1, R3, L4, R2, R4, L3, L1, R4, L4, R3, L5, L3, R188, R4, L1, R48, L5, R4, R71, R3, L2, R188, L3, R2, L3, R3, L5, L1, R1, L2, L4, L2, R5, L3, R3, R3, R4, L3, L4, R5, L4, L4, R3, R4, L4, R1, L3, L1, L1, R4, R1, L4, R1, L1, L3, R2, L2, R2, L1, R5, R3, R4, L5, R2, R5, L5, R1, R2, L1, L3, R3, R1, R3, L4, R4, L4, L1, R1, L2, L2, L4, R1, L3, R4, L2, R3, L1, L5, R4, R5, R2, R5, R1, R5, R1, R3, L3, L2, L2, L5, R2, L2, R5, R5, L2, R3, L5, R5, L2, R4, R2, L1, R3, L5, R3, R2, R5, L1, R3, L2, R2, R1"

data Direction 
    = North 
    | South
    | East
    | West

type Coord =
    { x :: Int
    , y :: Int
    }

type State =
    { facing :: Direction
    , pos :: Coord
    }

initialState =
    { facing : North
    , pos : { x : 0, y : 0 }
    }

extractNumSteps =
    drop 1 >>> fromString >>> fromMaybe 0

toTuple instr =
  let
      d = take 1 instr
      n = extractNumSteps instr
  in      
      Tuple d n

moveRight :: Direction -> Direction
moveRight North = East
moveRight East = South
moveRight South = West
moveRight West = North

moveLeft :: Direction -> Direction
moveLeft North = West
moveLeft West = South
moveLeft South = East
moveLeft East = North

changePos :: Direction -> Int -> Coord -> Coord
changePos North n {x,y} = {x:x, y:y+n}
changePos East n {x,y} = {x:x+n, y:y}
changePos South n {x,y} = {x:x, y:y-n}
changePos West n {x,y} = {x:x-n, y:y}

move :: State -> (Tuple String Int) -> State
move {facing, pos} (Tuple "R" n) = 
    let 
        d = moveRight facing
    in
        { facing : d, pos : changePos d n pos }
move {facing, pos} (Tuple "L" n) = 
    let 
        d = moveLeft facing
    in
        { facing : d, pos : changePos d n pos }
move state _ = state

parseInstructions =
    split (Pattern ", ") 
        >>> map toTuple 

distance {pos : {x,y}} =
    x + y

processInstructions inp =
    distance $ foldl move initialState $ parseInstructions inp

showState {facing, pos : { x, y }} =
    (case facing of
      North -> "N "
      East -> "E "
      South -> "S "
      West -> "W ") <> " (" <> (show x) <> ", " <> (show y) <> ")" 
