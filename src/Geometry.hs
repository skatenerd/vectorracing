module Geometry (Point(Point), Vector(Vector), vX, vY, pX, pY, distanceToSegment, segmentIntersects, makeSegment, translate, Segment(Segment), segmentPoints, segmentToVector, unitNormal, scale, distanceToPolyline, hitsPolyline) where

import Data.Maybe
import Data.Complex

data Line = Line Point Point deriving (Show)
-- Point should be parameterized by a numeric type
data Point = Point { pX :: Float, pY :: Float } deriving (Show, Eq)
data Segment = Segment Point Point deriving (Show)
data Range = Range { rLower :: Float, rUpper :: Float }
data Vector = Vector { vX :: Float, vY :: Float } deriving (Show)

lineToSegment (Line a b) = Segment a b
segmentToLine (Segment a b) = Line a b
vectorToPoint (Vector a b) = Point a b
mkVector (Point x y) = Vector x y

vnorm v = let (norm, _) = polar (vX v :+ vY v)
          in norm

segmentToVector segment@(Segment start end) = let reverseVector = scale (mkVector start) (-1)
                                                  Segment _ fnorb = translate segment reverseVector
                                                  difference = mkVector fnorb
                                               in difference
-- want point2vec
segmentPoints segment@(Segment start end) = let reverseVector = scale (mkVector start) (-1)
                                                Segment _ fnorb = translate segment reverseVector
                                                difference = mkVector fnorb
                                                intnorm = fromIntegral $ floor (vnorm difference)
                                                unitdifference = scale difference (1 / (vnorm difference))
                                                adscl n = translate start (scale unitdifference n)
                                            in fmap adscl [1..intnorm]

unitNormal segment = let v = segmentToVector segment
                         scaled = scale v (1 / (vnorm v))
                     in rotate scaled (pi / (-2))

class CartesianResident a where
  rotate :: a -> Float -> a
  getAngle :: a -> Float
  translate :: a -> Vector -> a

instance CartesianResident Point where
  rotate (Point x y) radians = let ascomplex = x :+ y
                                   (norm, startangle) = polar ascomplex
                                   newangle = startangle + radians
                                   newcomplex = mkPolar norm newangle
                                in Point (realPart newcomplex) (imagPart newcomplex)
  translate p v = Point (vX v + pX p) (vY v + pY p)
  getAngle (Point x y) = let ascomplex = x :+ y
                             (_, angle) = polar ascomplex
                         in angle

instance CartesianResident Vector where
  rotate v radians = mkVector (rotate (vectorToPoint v) radians)
  translate v delta = mkVector (translate (vectorToPoint v) delta)
  getAngle v = getAngle (vectorToPoint v)


instance CartesianResident Segment where
  rotate (Segment start end) radians = Segment (rotate start radians) (rotate end radians)
  translate (Segment start end) v = Segment (translate start v) (translate end v)
  getAngle segment@(Segment start end) = let reverseVector = scale (mkVector start) (-1)
                                             atOrigin = translate segment reverseVector
                                             (Segment _ newEnd) = atOrigin
                                         in getAngle newEnd

-- can we dry this up using composition?
instance CartesianResident Line where
  rotate line radians = segmentToLine (rotate (lineToSegment line) radians)
  translate line v = segmentToLine (translate (lineToSegment line) v)
  getAngle line = getAngle $ lineToSegment line

distance p1 p2 = sqrt $ square (pX p1 - pX p2) + square (pY p1 - pY p2)
                 where square x = x * x

scale (Vector x y) coefficient = Vector (coefficient * x) (coefficient * y)

intersection :: Line -> Line -> Point
intersection first second = let angleForFlatten = (-1) * (getAngle first)
                                rFirst = rotate first angleForFlatten
                                rSecond = rotate second angleForFlatten
                                Line (Point _ yvalue) _ = rFirst
                                xAxisCollide = inverseLine rSecond yvalue
                            in rotate (Point xAxisCollide yvalue) ((-1) * angleForFlatten)

inverseLine line@(Line start end) target = (pX end) + ((target - (pY end)) / (slope line))

perpendicular line throughPoint = let perpendicularSlope = (-1) * (1 / (slope line))
                                      delta = Vector 1 perpendicularSlope
                                      newEnd = translate throughPoint delta
                                  in Line throughPoint newEnd

distanceToSegment point segment = let p = closestPointOnSegment point segment
                                  in distance p point

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

distanceToPolyline p polyline = minimum $ map (distanceToSegment p) polyline

hitsPolyline segment polyline = any (segmentIntersects segment) polyline
