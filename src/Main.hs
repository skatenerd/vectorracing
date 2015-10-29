module Main where

import Debug.Trace

import Geometry
import Rendering
import GameTypes
import Course

import Data.Maybe
import Data.List

import Control.Monad.Loops
import Control.Monad.State
import Control.Monad.Reader

--import Debug.Trace

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

hitsShape segment shape = any (segmentIntersects segment) $ getWalls shape

hitsPolyline segment polyline = any (segmentIntersects segment) polyline
hitsCourse segment course = let (lb, rb) = boundaries course
                            in (hitsPolyline segment lb) || (hitsPolyline segment rb)

-- a "shape" is a collection of points whose order doesn't matter
getWalls shape = [makeSegment x y | x <- shape, y <- shape]

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



accelerate car direction = car {velocity = (add (vectorForDirection direction) (velocity car))}

noCourseTakeTurn state direction = let with_newVelocity = accelerate state direction
                                       afterCoast = coast with_newVelocity
                                    in afterCoast
takeCarTurn :: CarState -> Direction -> CarState
takeCarTurn state direction = newCoast (accelerate state direction)

updateHumanState state delta = state { humanState = takeCarTurn (humanState state) delta }
updateAIState state delta = state { aiState = takeCarTurn (aiState state) delta }


newCoast car = let new_position = translate (position car) (velocity car)
               in CarState new_position (velocity car) (position car)

lastSegmentTravelled carState = makeSegment (position carState) (priorPosition carState)

incstate :: StateT GameState (ReaderT GameConfig IO) ()
incstate = do
  now <- get
  --liftIO $ print now
  theConfig <- lift ask
  userInput <- liftIO promptInput
  let delta = inputToDelta userInput
      Just bestFutureNode = bestFutureState (getCourse theConfig) (aiState now) -- incomplete pattern mach!!
      InfiniTree (_, history) _ = bestFutureNode
  liftIO $ print history
  let (aIdirection, _) = head history
      withHumanMove = updateHumanState now delta
      withAIMove = updateAIState withHumanMove aIdirection
  put $ withAIMove
  newnow <- get
  liftIO $ putStrLn $ render newnow $ getCourse theConfig

crash = do
  gamestate <- get
  theConfig <- lift ask
  return $ hitsCourse (lastSegmentTravelled (humanState gamestate)) (getCourse theConfig)

startCarState = CarState (Point (-10) (-10)) (Vector 0 0) (Point (-10) (-10))
startAIState = CarState (Point (-12) (-12)) (Vector 0 0) (Point (-12) (-12))
startGameState = GameState {humanState = startCarState, aiState = startCarState}

squareBarrier = [[Point 3 3, Point 3 5, Point 5 5, Point 5 3], [Point (-8) (-8), Point (-8) 8], [Point (-8) 8, Point 8 8]]

thecourse = [(Point (-10) (-10)), (Point 0 0), (Point 10 2), (Point 35 (-5)), (Point 45 15)]

-- TODO: try reading some config values from a file...
-- TODO: use someting ncurses-like instead of reprinting everything
theconfig = GameConfig Course {path = thecourse, obstacles = squareBarrier}
main = putStrLn "\n\n\nWelcome!!!" >> (runReaderT (runStateT (untilM incstate crash) startGameState) $ theconfig)


carHasCrossed state segment = let zoom = lastSegmentTravelled state
                              in segmentIntersects zoom segment


-- AI
progressMarkers course = let lrps = getLeftrightPairs course
                         in map (uncurry Segment) lrps

indexOfLastElement elements predicate = let maybeDistanceFromEnd = ((flip findIndex) . reverse) elements predicate
                                            distanceFromEnd = fromMaybe (length elements) maybeDistanceFromEnd
                                        in (length elements) - distanceFromEnd

progress :: Course -> CarState -> Integer
progress course state = fromIntegral $ indexOfLastElement (progressMarkers course) (carHasCrossed state)

data InfiniTree a = InfiniTree { getValue :: a, getChildren :: [InfiniTree a] } deriving (Show)

theSubtrees state pathToHere = let makeForDirection d = makeFutureTree (takeCarTurn state d) (pathToHere ++ [(d, state)])
                               in map makeForDirection [Up, Down, LLeft, RRight]

makeFutureTree state pathToHere = InfiniTree (state, pathToHere) (theSubtrees state pathToHere)

bestNodeAtDepth 0 _ _ node = return node
bestNodeAtDepth depth score prune (InfiniTree value children) = argmax score (map (bestNodeAtDepth (depth - 1) score prune) (filter prune children))

argmax score elements = if null elements
                        then Nothing
                        else foldr takemax (head elements) elements
                        where takemax a b = if (score a) > (score b)
                                            then a
                                            else b

collidesWithCourse course node = let InfiniTree (_, howigothere) _ = node
                                     mypath = makeSegments $ map (position . snd) howigothere
                                     collision = any ((flip hitsCourse) course) mypath
                                 in collision

bestFutureState course state = bestNodeAtDepth 4 (scoreState course) (not . (collidesWithCourse course)) (makeFutureTree state [])

scoreState course Nothing = 0
scoreState course (Just node) = let InfiniTree (state, _) _ = node
                                    barsCrossed = progress course state
                                in barsCrossed
