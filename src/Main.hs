module Main where

import Debug.Trace

import Geometry

import Data.Maybe
import Data.List
import Data.Function

import Control.Monad.Loops
import Control.Monad.State
import Control.Monad.Reader

--import Debug.Trace

data CarState = CarState { position :: Point, velocity :: Vector, priorPosition :: Point} deriving (Show)
data GameState = GameState { humanState :: CarState, aiState :: CarState } deriving (Show)
data Direction = Up | UpRight | UpLeft | LLeft | RRight | DownRight | DownLeft | Down deriving (Show, Enum)
type Shape = [Point]

data Course = Course { path :: Shape, obstacles :: [Shape] }

glrp segment point = let normal = unitNormal segment
                         scaled = scale normal 5
                         inverted = scale scaled (-1)
                     in ((translate point inverted), (translate point scaled))


glrps :: Segment -> [(Point, Point)]
glrps segment = fmap (glrp segment) (segmentPoints segment)

makeSegments points = zipWith Segment points (tail points)

getLeftrightPairs points = concatMap glrps (makeSegments points)

boundaries points = let leftrightpairs = getLeftrightPairs points
                        (lefts, rights) = unzip leftrightpairs
                    in  (makeSegments lefts, makeSegments rights)

--progress point course = findIndex

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

distanceToPolyline p polyline = minimum $ map (distanceToSegment p) polyline
distanceToCourse p course = let (lb, rb) = boundaries (path course)
                            in min (distanceToPolyline p lb) (distanceToPolyline p rb)

hitsShape segment shape = any (segmentIntersects segment) $ getWalls shape

hitsPolyline segment polyline = any (segmentIntersects segment) polyline
hitsCourse segment course = let (lb, rb) = boundaries (path course)
                            in (hitsPolyline segment lb) || (hitsPolyline segment rb)

-- a "shape" is a collection of points whose order doesn't matter
getWalls shape = [makeSegment x y | x <- shape, y <- shape]

maybeOr a b = case a of
              Nothing -> b
              Just _ -> a

areSamePoint (x,y) p = ((Point `on` fromIntegral) x y) == p

placeCar x y gameState course = if areSamePoint (x,y) (position $ humanState gameState)
                                then Just "C"
                                else Nothing


placeAI x y gameState course = if areSamePoint (x,y) (position $ aiState gameState)
                               then Just "A"
                               else Nothing

placeEarth x y gameState course = Just "_"

placeCourse x y gameState course = if (distanceToCourse ((Point `on` fromIntegral) x y) course) < 0.5
                                  then Just "W"
                                  else Nothing

renderSquare :: Integer -> Integer -> GameState -> Course -> String
renderSquare x y gameState course = fromMaybe " " $ foldl maybeOr Nothing [placeCar x y gameState course,
                                                                          placeAI x y gameState course,
                                                                          placeCourse x y gameState course,
                                                                          placeEarth x y gameState course]

renderRow :: Integer -> GameState -> Course -> String
renderRow y state course = intercalate "" [renderSquare x y state course | x <- [-20..45]]

render :: GameState -> Course -> String
render state course = intercalate "\n" [renderRow y state course | y <- [15, 14 .. -15]]

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
  liftIO $ print now
  theConfig <- lift ask
  userInput <- liftIO promptInput
  let delta = inputToDelta userInput
  --TODO: EW!
  --let bestFutureNode = fst $ last $ snd $ getValue $ bestFutureState (getCourse theConfig) now
  let bestFutureNode = bestFutureState (getCourse theConfig) (aiState now)
      InfiniTree (imaginedState, history) _ = bestFutureNode
      (aIdirection, carState) = head history
      withHumanMove = updateHumanState now delta
      withAIMove = updateAIState withHumanMove aIdirection
  put $ withAIMove
  newnow <- get
  liftIO $ putStrLn $ render newnow $ getCourse theConfig
  liftIO $ print $ progress (getCourse theConfig) (aiState now)

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
progressMarkers course = let lrps = getLeftrightPairs (path course)
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

bestNodeAtDepth 0 _ node = node
bestNodeAtDepth depth score (InfiniTree value children) = argmax score (map (bestNodeAtDepth (depth - 1) score) children)

argmax score elements = foldr takemax (head elements) elements
                        where takemax a b = if (score a) > (score b)
                                            then a
                                            else b

-- add a 'prune' argument, because this is slow.  prune paths which crash into walls...
bestFutureState course state = bestNodeAtDepth 4 (scoreState course) (makeFutureTree state [])

scoreState course node = let InfiniTree (state, howigothere) _ = node
                             barsCrossed = progress course state
                             mypath = makeSegments $ map (position . snd) howigothere
                             collision = any ((flip hitsCourse) course) mypath
                         in if collision
                         then 0
                         else barsCrossed
