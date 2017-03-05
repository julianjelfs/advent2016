module Day1 where
   
import Prelude
import Data.Tuple
import Control.Bind ((=<<))
import Data.Int (fromString)
import Data.List (List, foldl)
import Data.Maybe (fromMaybe)
import Data.Newtype (overF)
import Data.Show (class Show)
import Data.String (Pattern(..), split, take, drop)

input :: String
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

initialState :: State
initialState =
    { facing : North
    , pos : { x : 0, y : 0 }
    }

extractNumSteps :: String -> Int
extractNumSteps =
    drop 1 >>> fromString >>> fromMaybe 0

toTuple :: String -> Tuple String Int
toTuple instr =
  let
      d = take 1 instr
      n = extractNumSteps instr
  in      
      Tuple d n

translateRight :: Direction -> Direction
translateRight North = East
translateRight East = South
translateRight South = West
translateRight West = North

translateLeft :: Direction -> Direction
translateLeft North = West
translateLeft West = South
translateLeft South = East
translateLeft East = North

moveRight :: State -> State
moveRight  s = s { facing = translateRight s.facing  }

moveLeft :: State -> State
moveLeft  s = s { facing = translateLeft s.facing  }

changePos :: Int -> State -> State
changePos n { facing, pos } =
    let 
        p = 
            case facing of 
                North -> pos { y = pos.y + n }
                East -> pos { x = pos.x + n }
                South -> pos { y = pos.y - n }
                West -> pos { x = pos.x - n }
    in
        { facing, pos : p } 

move :: State -> (Tuple String Int) -> State
move state (Tuple "R" n) = 
    moveRight state # changePos n
move state (Tuple "L" n) = 
    moveLeft state # changePos n
move state _ = state

parseInstructions :: String -> Array (Tuple String Int)
parseInstructions =
    split (Pattern ", ") 
        >>> map toTuple 

distance {pos : {x,y}} =
    x + y

processInstructions :: String -> Int
processInstructions inp =
    parseInstructions inp
        # foldl move initialState
        # distance

partOne :: Int
partOne =
    processInstructions input

partTwo :: Int
partTwo =
    processInstructions input

showState {facing, pos : { x, y }} =
    (case facing of
      North -> "N "
      East -> "E "
      South -> "S "
      West -> "W ") <> " (" <> (show x) <> ", " <> (show y) <> ")" 
