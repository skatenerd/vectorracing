module Geometry (Point(Point), pX, pY, distanceToSegment, segmentIntersects, makeSegment) where

import Data.Maybe

data Line = FunctionLine Float Float | VerticalLine Float deriving (Show)
data Point = Point { pX :: Float, pY :: Float } deriving (Show, Eq)
data Segment = Segment Point Point deriving (Show)
data Range = Range { rLower :: Float, rUpper :: Float }

lineThrough slope targetPoint = let yIntercept = pY targetPoint - (pX targetPoint * slope)
                                in FunctionLine slope yIntercept

-- Notice that valueAt is not defined for a verticalLine
-- LOL! Vertical lines aren't functions, so it's hard to talk about them in a functional language!!
valueAt (FunctionLine slope intercept) xValue = (slope * xValue) + intercept


yIntercept slope (Point x y) = y - (slope * x)


square x = x * x

distance p1 p2 = sqrt $ square (pX p1 - pX p2) + square (pY p1 - pY p2)

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




perpindicular (VerticalLine _) targetPoint = FunctionLine 0 $ pY targetPoint
perpindicular (FunctionLine slope intercept) targetPoint = if slope == 0
                                                           then VerticalLine $ pX targetPoint
                                                           else let perpindicularSlope = (-1) * (1 / slope)
                                                                in lineThrough perpindicularSlope targetPoint
closestPointOnLine point line = let perpindicularLine = perpindicular line point
                                    Just closestPointOnLine = intersection perpindicularLine line
                                in closestPointOnLine

distanceToSegment point segment = let p = closestPointOnLine point $ segmentToLine segment
                                      Segment alpha beta = segment
                                  in if pointInSegment p segment
                                  then distance p point
                                  else minimum $ map (distance point) [alpha, beta]


segmentToLine :: Segment -> Line
segmentToLine segment = if isVertical segment
                    then let Segment (Point x _) _ = segment
                         in VerticalLine x
                    else let theslope = segmentSlope segment
                             Segment point _ = segment
                             intercept = yIntercept theslope point
                         in FunctionLine theslope intercept

segmentIntersection :: Segment -> Segment -> Maybe Point
segmentIntersection firstSegment secondSegment =
  let firstLine = segmentToLine firstSegment
      secondLine = segmentToLine secondSegment
  in do
    -- monads!
    theIntersection <- intersection firstLine secondLine
    forceIntoDomains theIntersection [firstSegment, secondSegment]



segmentSlope :: Segment -> Float
segmentSlope (Segment from to) =
  let rise = pY to - pY from
      run = pX to - pX from
  in (rise / run)


makeSegment = Segment

isVertical (Segment from to) = pX from == pX to

segmentIntersects :: Segment -> Segment -> Bool
segmentIntersects first second = let intersection = segmentIntersection first second
                                 in isJust intersection

inRange value range =
   (value <= rUpper range) && (value >= rLower range)

pointInSegment point (Segment from to) = (inRange (pX point) $ makeRange (pX from) (pX to)) &&
                                         (inRange (pY point) $ makeRange (pY from) (pY to))

inSegments :: Point -> [Segment] -> Bool
inSegments point = all (pointInSegment point)


forceIntoDomains point segments = if inSegments point segments then Just point else Nothing


makeRange :: Float -> Float -> Range
makeRange first second = if first < second then Range first second else Range second first
