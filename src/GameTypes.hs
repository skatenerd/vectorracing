module GameTypes where

import Geometry

data GameColors = Dust | Car | Earth | Wall | Road | Opponent deriving (Show, Eq)

data CarState = CarState { positionHistory :: [Point] } deriving (Show)
data GameState = GameState { humanState :: CarState, aiState :: CarState, quitted :: Bool} deriving (Show)

data Direction = Up | UpRight | UpLeft | LLeft | RRight | DownRight | DownLeft | Down deriving (Show, Enum, Eq)

type Shape = [Point]

data Course = Course { path :: Shape, obstacles :: [Shape], getBoundaries :: ([Segment], [Segment]), getLeftrightPairs :: [(Point, Point)] }
data Command = MoveDirection Direction | Quit deriving (Show, Eq)

