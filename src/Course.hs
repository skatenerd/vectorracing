module Course (getLeftrightPairs, boundaries, makeSegments, distanceToCourse, pointsAlong, onRoad) where

import Geometry
import GameTypes
import Data.List.Extras

boundaries course = let leftrightpairs = getLeftrightPairs course
                        (lefts, rights) = unzip leftrightpairs
                    in  (makeSegments lefts, makeSegments rights)


glrp segment point = let normal = unitNormal segment
                         scaled = scale normal courseWidth
                         inverted = scale scaled (-1)
                     in ((translate point inverted), (translate point scaled))

courseWidth = 5

glrps :: Segment -> [(Point, Point)]
glrps segment = fmap (glrp segment) (segmentPoints segment)

pointsAlong course = concatMap segmentPoints (makeSegments (path course))

makeSegments points = zipWith Segment points (tail points)

getLeftrightPairs course = concatMap glrps (makeSegments (path course))

distanceToCourse p course = let (lb, rb) = boundaries course
                            in min (distanceToPolyline p lb) (distanceToPolyline p rb)

onRoad p course = let (lb, rb) = boundaries course
                      leftBoundaryHit = closestPointOnPolyline p lb
                      rightBoundaryHit = closestPointOnPolyline p rb
                  in between p leftBoundaryHit rightBoundaryHit

