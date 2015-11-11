
module GameTypes where
import Data.RTree (RTree)

import Geometry

data GameColors = Earth | Road | Finish | Wall | Dust | Opponent | Car deriving (Show, Eq, Ord)

data CarState = CarState { positionHistory :: [Point] } deriving (Show)

data GameState = GameState { humanState :: CarState, aiState :: CarState, quitted :: Bool} deriving (Show)

data Direction = Up | UpRight | UpLeft | LLeft | RRight | DownRight | DownLeft | Down deriving (Show, Enum, Eq)

type Shape = [Point]

data Course = Course { path :: Shape,
                     obstacles :: [Shape],
                     getBoundaries :: ([Segment], [Segment]),
                     getLeftrightPairs :: [(Point, Point)],
                     getBoundaryCache :: RTree Point,
                     getPathCache :: RTree Point}

data Command = MoveDirection Direction | Quit deriving (Show, Eq)

