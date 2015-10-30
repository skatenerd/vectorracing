module GameTypes where

import Geometry

data CarState = CarState { position :: Point, velocity :: Vector, priorPosition :: Point} deriving (Show)
data GameState = GameState { humanState :: CarState, aiState :: CarState } deriving (Show)

data Direction = Up | UpRight | UpLeft | LLeft | RRight | DownRight | DownLeft | Down deriving (Show, Enum, Eq)

type Shape = [Point]

data Course = Course { path :: Shape, obstacles :: [Shape] }
