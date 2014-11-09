module Main where
import Data.Bool
data Point = Point { pX :: Float, pY :: Float } deriving (Show)
data Vector = Vector { vX :: Float, vY :: Float } deriving (Show)
data Segment = Segment Point Point deriving (Show)

data Line = FunctionLine Float Float | VerticalLine Float deriving (Show)

data Range = Range { rLower :: Float, rUpper :: Float }
data CarState = CarState { position :: Point, velocity :: Vector} deriving (Show)
data Direction = Up | UpRight | UpLeft | LLeft | RRight | DownRight | DownLeft | Down
type Shape = [Point]
type Course = [Shape]

lineThrough slope targetPoint = let yIntercept = (pY targetPoint) - ((pX targetPoint) * slope)
                                in FunctionLine slope yIntercept

perpindicular (VerticalLine _) targetPoint = FunctionLine 0 $ pY targetPoint
perpindicular (FunctionLine slope intercept) targetPoint = if slope == 0
                                                           then VerticalLine $ pX targetPoint
                                                           else let perpindicularSlope = (-1) * (1 / slope)
                                                                in lineThrough perpindicularSlope targetPoint
square x = x * x

distance p1 p2 = sqrt $ (square (pX p1 - pX p2)) + (square (pY p1 - pY p2))

distanceToLine point line = let p = perpindicular line point
                                Just closestPointOnLine = intersection p line
                            in distance point closestPointOnLine

add :: Vector -> Vector -> Vector
add v1 v2 = Vector (vX v1 + vX v2) (vY v1 + vY v2)

translate p v = Point (vX v + pX p) (vY v + pY p)

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

hitsShape segment shape = let walls = zipWith makeSegment shape (tail shape ++ [head shape])
                           in any (segmentIntersects segment) walls

hitsCourse segment = any (hitsShape segment)

accelerate car direction = CarState (position car) (add (vectorForDirection direction) (velocity car))

takeTurn state direction course = let with_newVelocity = accelerate state direction
                                      afterCoast = coast with_newVelocity
                                      segmentCreated = makeSegment (position state) (position afterCoast)
                                   in if hitsCourse segmentCreated course then
                                         Nothing else
                                         Just afterCoast

inRange value range =
   (value < rUpper range) && (value > rLower range)

makeRange :: Float -> Float -> Range
makeRange first second = if first < second then Range first second else Range second first

inDomain xValue (Segment from to) = inRange xValue $ makeRange (pX from) (pX to)

inDomains :: Float -> [Segment] -> Bool
inDomains point = all (inDomain point)

forceIntoDomains point segments = if inDomains (pX point) segments then Just point else Nothing

segmentSlope :: Segment -> Float
segmentSlope (Segment from to) =
  let rise = pY to - pY from
      run = pX to - pX from
  in (rise / run)

yIntercept slope (Point x y) = y - (slope * x)

makeSegment = Segment

isVertical (Segment from to) = pX from == pX to

segmentToLine :: Segment -> Line
segmentToLine segment = if isVertical segment
                    then let Segment (Point x _) _ = segment
                         in VerticalLine x
                    else let theslope = segmentSlope segment
                             Segment point _ = segment
                             intercept = yIntercept theslope point
                         in FunctionLine theslope intercept

-- Notice that valueAt is not defined for a verticalLine
-- LOL! Vertical lines aren't functions, so it's hard to talk about them in a functional language!!
valueAt (FunctionLine slope intercept) xValue = (slope * xValue) + intercept

intersection :: Line -> Line -> Maybe Point

intersection (VerticalLine _) (VerticalLine _) = Nothing
intersection functionLine@FunctionLine{} (VerticalLine i) = Just $ Point i (valueAt functionLine i)
intersection (VerticalLine i) functionLine@FunctionLine{} = Just $ Point i (valueAt functionLine i)
intersection (FunctionLine firstSlope firstIntercept) (FunctionLine secondSlope secondIntercept) =
  if firstSlope == secondSlope
  then Nothing
  else let lhsCoefficient = firstSlope - secondSlope
           rhs = secondIntercept - firstIntercept
           xIntersection = rhs / lhsCoefficient
           yIntersection = valueAt (FunctionLine firstSlope firstIntercept) xIntersection
  in Just $ Point xIntersection yIntersection

segmentIntersection :: Segment -> Segment -> Maybe Point
segmentIntersection firstSegment secondSegment =
  let firstLine = segmentToLine firstSegment
      secondLine = segmentToLine secondSegment
  in do
    theIntersection <- intersection firstLine secondLine
    forceIntoDomains theIntersection [firstSegment, secondSegment]

segmentIntersects :: Segment -> Segment -> Bool
segmentIntersects first second = let intersection = segmentIntersection first second
                                  in case intersection of Nothing -> False
                                                          (Just _) -> True

main = let first = Point 2 0
           second = Point 3 2.5
           diagonalSegment = Segment first second
           extendedSegment = segmentToLine diagonalSegment
           flatSegment = Segment (Point 5 5) (Point 10 5)
           flatLine = segmentToLine flatSegment
           verticalLine = segmentToLine $ Segment (Point 2 0) (Point 2 10)
           square_barrier = [Point (-8) (-8), Point (-8) 8, Point 8 8, Point 8 (-8)]
           squareCourse = [square_barrier]
           initialCarState = CarState (Point 0 0) (Vector 0 0)
           Just after_right = takeTurn initialCarState RRight squareCourse
           Just after_right_up = takeTurn after_right Up squareCourse
           Just after_right_up_up = takeTurn after_right_up Up squareCourse
           Just after_right_up_up_up = takeTurn after_right_up_up Up squareCourse
           offThe_end = takeTurn after_right_up_up_up Up squareCourse
 in do
  print diagonalSegment
  print $ segmentSlope diagonalSegment
  print $ yIntercept (segmentSlope diagonalSegment) first
  print $ segmentToLine diagonalSegment
  print flatLine
  print $ distance (Point 0 0) (Point 1 1)
  print $ lineThrough 2 $ Point 0 5
  print $ perpindicular (lineThrough 2 $ Point 0 5) (Point 0 5)
  print $ distanceToLine (Point 0 0) (FunctionLine (-1) 2)
  print verticalLine
  print after_right_up_up_up
  print offThe_end
