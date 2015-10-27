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

data CarState = CarState { position :: Point, velocity :: Vector} deriving (Show)
data GameState = GameState {getCarState :: CarState, lastCarState :: CarState} deriving (Show)
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

accelerate car direction = CarState (position car) (add (vectorForDirection direction) (velocity car))

noCourseTakeTurn state direction = let with_newVelocity = accelerate state direction
                                       afterCoast = coast with_newVelocity
                                    in afterCoast

maybeOr a b = case a of
              Nothing -> b
              Just _ -> a

placeCar x y carState course = if (position carState) == (Point `on` fromIntegral) x y
                               then Just "C"
                               else Nothing

placeEarth x y carState course = Just "_"

placeCourse x y carState course = if (distanceToCourse ((Point `on` fromIntegral) x y) course) < 0.5
                                  then Just "W"
                                  else Nothing

renderSquare :: Integer -> Integer -> CarState -> Course -> String
renderSquare x y carState course = fromMaybe " " $ foldl maybeOr Nothing [placeCar x y carState course,
                                                                          placeCourse x y carState course,
                                                                          placeEarth x y carState course]

renderRow :: Integer -> CarState -> Course -> String
renderRow y state course = intercalate "" [renderSquare x y state course | x <- [-20..20]]

render :: CarState -> Course -> String
render state course = intercalate "\n" [renderRow y state course | y <- [20, 19 .. -20]]

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
  theConfig <- lift ask
  --userInput <- liftIO promptInput
  --let delta = inputToDelta userInput
  --TODO: EW!
  let delta = fst $ last $ snd $ getValue $ bestFutureState (getCourse theConfig) now
  put $ GameState (noCourseTakeTurn (getCarState now) delta) (getCarState now)
  newnow <- get
  liftIO $ putStrLn $ render (getCarState newnow) $ getCourse theConfig
  liftIO $ print $ progress (getCourse theConfig) now

crash = do
  gamestate <- get
  theConfig <- lift ask
  return $ hitsCourse (makeSegment (position $ getCarState gamestate) (position $ lastCarState gamestate)) (getCourse theConfig)

startCarState = CarState (Point (-10) (-10)) (Vector 0 0)
startGameState = GameState startCarState startCarState

squareBarrier = [[Point 3 3, Point 3 5, Point 5 5, Point 5 3], [Point (-8) (-8), Point (-8) 8], [Point (-8) 8, Point 8 8]]

thecourse = [(Point (-10) (-10)), (Point 0 0), (Point 1 15)]

-- TODO: try reading some config values from a file...
-- TODO: use someting ncurses-like instead of reprinting everything
theconfig = GameConfig Course {path = thecourse, obstacles = squareBarrier}
main = putStrLn "\n\n\nWelcome!!!" >> (runReaderT (runStateT (untilM incstate crash) startGameState) $ theconfig)


carHasCrossed state segment = let zoom = Segment (position (getCarState state)) (position (lastCarState state))
                              in segmentIntersects zoom segment








-- AI
progressMarkers course = let lrps = getLeftrightPairs (path course)
                         in map (uncurry Segment) lrps

indexOfLastElement elements predicate = let maybeDistanceFromEnd = ((flip findIndex) . reverse) elements predicate
                                            distanceFromEnd = fromMaybe (length elements) maybeDistanceFromEnd
                                        in (length elements) - distanceFromEnd

progress :: Course -> GameState -> Integer
progress course state = fromIntegral $ indexOfLastElement (progressMarkers course) (carHasCrossed state)

data InfiniTree a = InfiniTree { getValue :: a, getChildren :: [InfiniTree a] } deriving (Show)


updateStateForMove state direction = GameState (noCourseTakeTurn (getCarState state) direction) (getCarState state)

theSubtrees state backwardsPathToHere = let makeForDirection d = makeFutureTree (updateStateForMove state d)  ((d, getCarState state) : backwardsPathToHere)
                                        in map makeForDirection [Up ..]

makeFutureTree state backwardsPathToHere = InfiniTree (state, backwardsPathToHere) (theSubtrees state backwardsPathToHere)

bestNodeAtDepth 0 _ node = node
bestNodeAtDepth depth score (InfiniTree value children) = argmax score (map (bestNodeAtDepth (depth - 1) score) children)

argmax score elements = foldr takemax (head elements) elements
                        where takemax a b = if (score a) > (score b)
                                            then a
                                            else b

bestFutureState course state = bestNodeAtDepth 4 (scoreState course) (makeFutureTree state [])

scoreState course node = let InfiniTree (state, howigothere) _ = node
                             barsCrossed = progress course state
                             mypath = makeSegments $ map (position . snd) howigothere
                             collision = any ((flip hitsCourse) course) mypath
                         in if collision
                         then 0
                         else barsCrossed
