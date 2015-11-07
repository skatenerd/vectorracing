module Course (makeCourse, makeSegments, distanceToCourse, pointsAlong, onRoad, courseWidth) where

import Geometry
import GameTypes
import Data.List.Extras

makeCourse path obstacles = Course { path = path, obstacles = obstacles, getBoundaries = computeBoundaries path, getLeftrightPairs = computeLeftrightPairs path }

computeBoundaries path = let leftrightpairs = computeLeftrightPairs path
                             (lefts, rights) = unzip leftrightpairs
                         in (makeSegments lefts, makeSegments rights)

courseWidth = 5


pointsAlong course = concatMap segmentPoints (makeSegments (path course))

makeSegments points = zipWith Segment points (tail points)

computeLeftrightPairs path = concatMap pairsForSegment (makeSegments path)
  where pairsForSegment segment = fmap (makeLeftRightPair segment) (segmentPoints segment)
        makeLeftRightPair segment point = let normal = unitNormal segment
                                              scaled = scale normal courseWidth
                                              inverted = scale scaled (-1)
                                          in (translate point inverted, translate point scaled)

distanceToCourse p course = let (lb, rb) = getBoundaries course
                            in min (distanceToPolyline p lb) (distanceToPolyline p rb)

onRoad p course = let (lb, rb) = getBoundaries course
                      leftBoundaryHit = closestPointOnPolyline p lb
                      rightBoundaryHit = closestPointOnPolyline p rb
                  in between p leftBoundaryHit rightBoundaryHit

