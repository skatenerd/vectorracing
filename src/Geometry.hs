module Geometry (Point(Point), Vector(Vector), vX, vY, pX, pY, distanceToSegment, segmentIntersects, makeSegment, translate) where

import Data.Maybe
import Data.Complex

data Line = Line Point Point deriving (Show)
data Point = Point { pX :: Float, pY :: Float } deriving (Show, Eq)
data Segment = Segment Point Point deriving (Show)
data Range = Range { rLower :: Float, rUpper :: Float }
data Vector = Vector { vX :: Float, vY :: Float } deriving (Show)

translate p v = Point (vX v + pX p) (vY v + pY p)

square x = x * x

distance p1 p2 = sqrt $ square (pX p1 - pX p2) + square (pY p1 - pY p2)

mkVector (Point x y) = Vector x y
scale (Vector x y) coefficient = Vector (coefficient * x) (coefficient * y)

--rotate :: (Num a) => Line -> a -> Line
rotate (Line start end) radians = Line (rotatePoint start radians) (rotatePoint end radians)

rotatePoint (Point x y) radians = let ascomplex = x :+ y
                                      (norm, startangle) = polar ascomplex
                                      newangle = startangle + radians
                                      newcomplex = mkPolar norm newangle
                                   in Point (realPart newcomplex) (imagPart newcomplex)

getAngle (Point x y) = let ascomplex = x :+ y
                           (_, angle) = polar ascomplex
                       in angle

inputFor line@(Line start end) target = (pX end) + ((target - (pY end)) / (slope line))

intersection :: Line -> Line -> Point
intersection first second = let (Line start end) = first
                                theta = getAngle $ translate end (scale (mkVector start) (-1))
                                negTheta = (-1) * theta
                                rFirst = rotate first negTheta
                                rSecond = rotate second negTheta
                                Line (Point _ yvalue) _ = rFirst
                                xAxisCollide = inputFor rSecond yvalue
                            in rotatePoint (Point xAxisCollide yvalue) theta

perpendicular line throughPoint = let perpendicularSlope = (-1) * (1 / (slope line))
                                      delta = Vector 1 perpendicularSlope
                                      newEnd = translate throughPoint delta
                                  in Line throughPoint newEnd

closestPointOnLine point line = let perpendicularLine = perpendicular line point
                                    closestPointOnLine = intersection perpendicularLine line
                                in closestPointOnLine


distanceToSegment point segment = let p = closestPointOnLine point $ segmentToLine segment
                                      Segment alpha beta = segment
                                  in if pointInSegment p segment
                                  then distance p point
                                  else minimum $ map (distance point) [alpha, beta]


segmentToLine :: Segment -> Line
segmentToLine (Segment a b) = Line a b

segmentIntersection :: Segment -> Segment -> Maybe Point
segmentIntersection firstSegment secondSegment =
  let firstLine = segmentToLine firstSegment
      secondLine = segmentToLine secondSegment
      theIntersection = intersection firstLine secondLine
  in
      forceIntoDomains theIntersection [firstSegment, secondSegment]

slope (Line from to) =
  let rise = pY to - pY from
      run = pX to - pX from
  in (rise / run)

makeSegment = Segment

segmentIntersects :: Segment -> Segment -> Bool
segmentIntersects first second = let intersection = segmentIntersection first second
                                 in isJust intersection

epsilon = 0.0001

inRange value range epsilon =
   (value <= (rUpper range) + epsilon) && (value >= (rLower range) - epsilon)


pointInSegment point (Segment from to) = (inRange (pX point) (makeRange (pX from) (pX to)) epsilon) &&
                                         (inRange (pY point) (makeRange (pY from) (pY to)) epsilon)

inSegments :: Point -> [Segment] -> Bool
inSegments point = all (pointInSegment point)


forceIntoDomains point segments = if inSegments point segments then Just point else Nothing


makeRange :: Float -> Float -> Range
makeRange first second = if first < second then Range first second else Range second first
