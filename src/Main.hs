module Main where

import Geometry

import Data.Maybe
import Data.List

import Control.Monad.Loops
import Control.Monad.State
import Control.Monad.Reader

--import Debug.Trace

data CarState = CarState { position :: Point, velocity :: Vector} deriving (Show)
data GameState = GameState {getCarState :: CarState, lastCarState :: CarState} deriving (Show)
data Direction = Up | UpRight | UpLeft | LLeft | RRight | DownRight | DownLeft | Down deriving (Show)
type Shape = [Point]
type Course = [Shape]

data GameConfig = GameConfig { getCourse :: Course }


add :: Vector -> Vector -> Vector
add v1 v2 = Vector (vX v1 + vX v2) (vY v1 + vY v2)


coast car = let new_position = translate (position car) (velocity car)
            in CarState new_position (velocity car)

vectorForDirection :: Direction -> Vector
vectorForDirection Up = Vector 0 1
vectorForDirection UpRight = Vector 1 1
vectorForDirection UpLeft = Vector (-1) 1
vectorForDirection LLeft = Vector (-1) 0
vectorForDirection RRight = Vector 1 0
vectorForDirection DownRight = Vector 1 (-1)
vectorForDirection DownLeft = Vector (-1) (-1)
vectorForDirection Down = Vector 0 (-1)


distanceToShape p shape = minimum (map (distanceToSegment p) (getWalls shape))

distanceToCourse p course = minimum (map (distanceToShape p) course)

hitsShape segment shape = any (segmentIntersects segment) $ getWalls shape

hitsCourse segment = any (hitsShape segment)

getWalls shape = [makeSegment x y | x <- shape, y <- shape]

accelerate car direction = CarState (position car) (add (vectorForDirection direction) (velocity car))

takeTurn state direction course = let with_newVelocity = accelerate state direction
                                      afterCoast = coast with_newVelocity
                                      segmentCreated = makeSegment (position state) (position afterCoast)
                                   in if hitsCourse segmentCreated course then
                                         Nothing else
                                         Just afterCoast

noCourseTakeTurn state direction = let with_newVelocity = accelerate state direction
                                       afterCoast = coast with_newVelocity
                                   in afterCoast



updateNth :: [a] -> Int -> (a -> a) -> [a]
updateNth (first:rest) 0 howToChange = (howToChange first) : rest
updateNth (first:rest) index howToChange = first : updateNth rest (index - 1) howToChange
updateNth stuff _ _ = stuff

maybeOr a b = case a of
              Nothing -> b
              Just _ -> a

placeCar x y carState course = if (position carState) == Point x y
                               then Just "C"
                               else Nothing

placeEarth x y carState course = Just "_"

placeCourse x y carState course = if (distanceToCourse (Point x y) course) < 0.01
                                  then Just "W"
                                  else Nothing

-- this should be a fold...
renderSquare x y carState course = fromJust $ maybeOr (maybeOr (placeCar x y carState course) (placeCourse x y carState course)) (placeEarth x y carState course)

renderRow y state course = intercalate "" [renderSquare x y state course | x <- [-10..10]]

render state course = intercalate "\n" [renderRow y state course | y <- [10, 9 .. -10]]

inputToDelta "up" = Up
inputToDelta "upright" = UpRight
inputToDelta "upleft" = UpLeft
inputToDelta "left" = LLeft
inputToDelta "right" = RRight
inputToDelta "downright" = DownRight
inputToDelta "downleft" = DownLeft
inputToDelta "down" = Down
inputToDelta _ = Up


promptInput = let valid_inputs = ["up", "upright", "upleft", "left", "right", "downright", "downleft", "down"]
              in do
                putStrLn "\n\nPlease enter a direction"
                userInput <- getLine
                putStrLn userInput
                if elem userInput valid_inputs
                  then return userInput
                  else do
                    putStrLn "Please enter a legal input"
                    print valid_inputs
                    promptInput

incstate :: StateT GameState (ReaderT GameConfig IO) ()
incstate = do
  now <- get
  liftIO $ print now
  userInput <- liftIO promptInput
  theConfig <- lift ask
  let delta = inputToDelta userInput
  put $ GameState (noCourseTakeTurn (getCarState now) delta) (getCarState now)
  newnow <- get
  liftIO $ putStrLn $ render (getCarState newnow) $ getCourse theConfig

crash = do
  gamestate <- get
  theConfig <- lift ask
  return $ hitsCourse (makeSegment (position $ getCarState gamestate) (position $ lastCarState gamestate)) (getCourse theConfig)

startCarState = CarState (Point 0 0) (Vector 0 0)
startGameState = GameState startCarState startCarState

squareBarrier = [[Point 3 3, Point 3 5, Point 5 5, Point 5 3], [Point (-8) (-8), Point (-8) 8], [Point (-8) 8, Point 8 8]]

-- TODO: try reading some config values from a file...
-- TODO: use someting ncurses-like instead of reprinting everything
main = putStrLn "\n\n\nWelcome!!!" >> (runReaderT (runStateT (untilM incstate crash) startGameState) $ GameConfig squareBarrier)
