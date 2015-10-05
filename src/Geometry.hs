module Geometry (Point(Point), Vector(Vector), vX, vY, pX, pY, distanceToSegment, segmentIntersects, makeSegment, translate) where

import Data.Maybe
import Data.Complex

data Line = Line Point Point deriving (Show)
data Point = Point { pX :: Float, pY :: Float } deriving (Show, Eq)
data Segment = Segment Point Point deriving (Show)
data Range = Range { rLower :: Float, rUpper :: Float }
data Vector = Vector { vX :: Float, vY :: Float } deriving (Show)

class Rotatable a where
  rotate :: a -> Float -> a

instance Rotatable Point where
  rotate (Point x y) radians = let ascomplex = x :+ y
                                   (norm, startangle) = polar ascomplex
                                   newangle = startangle + radians
                                   newcomplex = mkPolar norm newangle
                                in Point (realPart newcomplex) (imagPart newcomplex)

instance Rotatable Line where
  rotate (Line start end) radians = Line (rotate start radians) (rotate end radians)

instance Rotatable Segment where
  rotate (Segment start end) radians = Segment (rotate start radians) (rotate end radians)

translate p v = Point (vX v + pX p) (vY v + pY p)

distance p1 p2 = sqrt $ square (pX p1 - pX p2) + square (pY p1 - pY p2)
                 where square x = x * x

mkVector (Point x y) = Vector x y
scale (Vector x y) coefficient = Vector (coefficient * x) (coefficient * y)

getAngle (Point x y) = let ascomplex = x :+ y
                           (_, angle) = polar ascomplex
                       in angle

intersection :: Line -> Line -> Point
intersection first second = let angleForFlatten = (-1) * (segmentAngle first)
                                rFirst = rotate first angleForFlatten
                                rSecond = rotate second angleForFlatten
                                Line (Point _ yvalue) _ = rFirst
                                xAxisCollide = inverseLine rSecond yvalue
                            in rotate (Point xAxisCollide yvalue) ((-1) * angleForFlatten)

segmentAngle (Line start end) = getAngle $ translate end (scale (mkVector start) (-1))
inverseLine line@(Line start end) target = (pX end) + ((target - (pY end)) / (slope line))

perpendicular line throughPoint = let perpendicularSlope = (-1) * (1 / (slope line))
                                      delta = Vector 1 perpendicularSlope
                                      newEnd = translate throughPoint delta
                                  in Line throughPoint newEnd

distanceToSegment point segment = let p = closestPointOnSegment point segment
                                  in distance p point

segmentToLine :: Segment -> Line
segmentToLine (Segment a b) = Line a b

inSegments :: Point -> [Segment] -> Bool
inSegments point = all (pointInSegment point)
                   where pointInSegment point s@(Segment from to) = distanceToSegment point s < 0.01

slope (Line from to) =
  let rise = pY to - pY from
      run = pX to - pX from
  in (rise / run)

makeSegment = Segment

segmentIntersects :: Segment -> Segment -> Bool
segmentIntersects first second = let intersection = segmentIntersection first second
                                 in isJust intersection

segmentIntersection :: Segment -> Segment -> Maybe Point
segmentIntersection firstSegment secondSegment =
  let firstLine = segmentToLine firstSegment
      secondLine = segmentToLine secondSegment
      theIntersection = intersection firstLine secondLine
  in if inSegments theIntersection [firstSegment, secondSegment]
     then Just theIntersection
     else Nothing

pushIntoRange value range = min (max value (rLower range)) (rUpper range)
xRange (Segment from to) = makeRange (pX from) (pX to)
yRange (Segment from to) = makeRange (pY from) (pY to)
closestPointOnSegment point segment = let perpendicularLine = perpendicular (segmentToLine segment) point
                                          hit = intersection perpendicularLine (segmentToLine segment)
                                          pushIntoSegment point segment = Point (pushIntoRange (pX point) (xRange segment))
                                                                                (pushIntoRange (pY point) (yRange segment))
                                      in pushIntoSegment hit segment

makeRange :: Float -> Float -> Range
makeRange first second = if first < second then Range first second else Range second first
