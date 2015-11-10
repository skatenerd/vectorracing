module Geometry (Point(Point), Vector(Vector), vX, vY, pX, pY, distanceToSegment, segmentIntersects, segmentIntersectsGenerous, makeSegment, translate, Segment(Segment), segmentPoints, segmentToVector, unitNormal, scale, distanceToPolyline, hitsPolyline, scaleSegment, vnorm, dot, distance, closestPointOnPolyline, between, pointDifference, makeBoundingBox) where

import Data.Maybe
import Data.Complex
import Data.Function
import Data.List.Extras

import qualified Data.RTree as RT
import GHC.Float

data Line = Line Point Point deriving (Show)
-- Point should be parameterized by a numeric type
data Point = Point { pX :: Float, pY :: Float } deriving (Show, Eq)
data Segment = Segment Point Point deriving (Show, Eq)
data Range = Range { rLower :: Float, rUpper :: Float }
data Vector = Vector { vX :: Float, vY :: Float } deriving (Show)

lineToSegment (Line a b) = Segment a b
segmentToLine (Segment a b) = Line a b
vectorToPoint (Vector a b) = Point a b
mkVector (Point x y) = Vector x y

vnorm v = let (norm, _) = polar (vX v :+ vY v)
          in norm

scaleSegment segment@(Segment start end) c = Segment start $ translate end $ scale (segmentToVector segment) c

segmentToVector segment@(Segment start end) = pointDifference end start

-- dry me up with utiliy functions
segmentPoints segment@(Segment start end) = let difference = segmentToVector segment
                                                intnorm = fromIntegral $ floor (vnorm difference)
                                                unitdifference = scale difference (1 / (vnorm difference))
                                                addScaled n = translate start (scale unitdifference n)
                                            in fmap addScaled [0..intnorm]

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

distance p1 p2 = sqrt $ squaredDistance p1 p2
--distance p = sqrt . (squaredDistance p)

squaredDistance p1 p2 = square (pX p1 - pX p2) + square (pY p1 - pY p2)
                        where square x = x * x

scale (Vector x y) coefficient = Vector (coefficient * x) (coefficient * y)
invert v = scale v (-1)
vectorDifference first second = translate first (invert second)
pointDifference = (vectorDifference `on` mkVector)

vectorFormat line = let Line start end = line
                    in (start, pointDifference end start)

cross first second = ((vX first) * (vY second)) - ((vY first) * (vX second))
dot first second = (vX first) * (vX second) + (vY first) * (vY second)

intersection :: Line -> Line -> Point
intersection first second = let (firstBase, firstDelta) = vectorFormat first
                                (secondBase, secondDelta) = vectorFormat second
                                baseDelta = pointDifference secondBase firstBase
                                coefficientForFirstDelta = (cross baseDelta secondDelta) / (cross firstDelta secondDelta)
                            in translate firstBase (scale firstDelta coefficientForFirstDelta)

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

segmentIntersectsGenerous first@(Segment fStart fEnd) second@(Segment sStart sEnd) =
  let endsAlign = any id $ do
      a <- [fStart, fEnd]
      b <- [sStart, sEnd]
      return (a == b)
  in endsAlign || (segmentIntersects first second)


segmentIntersection :: Segment -> Segment -> Maybe Point
segmentIntersection firstSegment secondSegment =
  let theIntersection = (intersection `on` segmentToLine) firstSegment secondSegment
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

closestPointOnPolyline p polyline = closestPointOnSegment p closestSegment
  where closestSegment = argmin (distanceSquaredToSegment p) polyline
        distanceSquaredToSegment p segment = squaredDistance p (closestPointOnSegment p segment)

between p first second = let baseline = pointDifference second first
                             candidate = pointDifference p first
                             dotted = dot baseline candidate
                             dotPositive = dotted > 0
                             dotSmallEnough = dotted < threshold
                             threshold = squaredDistance first second
                         in dotPositive && dotSmallEnough

makeBoundingBox sideLength point = RT.mbb (floatX - halfSideLength) (floatY - halfSideLength) (floatX + halfSideLength) (floatY + halfSideLength)
  where floatX = float2Double $ pX point
        floatY = float2Double $ pY point
        halfSideLength = sideLength / 2
