module Course (getLeftrightPairs, boundaries, makeSegments, distanceToCourse) where

import Geometry
import GameTypes

boundaries course = let leftrightpairs = getLeftrightPairs course
                        (lefts, rights) = unzip leftrightpairs
                    in  (makeSegments lefts, makeSegments rights)


glrp segment point = let normal = unitNormal segment
                         scaled = scale normal 5
                         inverted = scale scaled (-1)
                     in ((translate point inverted), (translate point scaled))


glrps :: Segment -> [(Point, Point)]
glrps segment = fmap (glrp segment) (segmentPoints segment)

makeSegments points = zipWith Segment points (tail points)

getLeftrightPairs course = concatMap glrps (makeSegments (path course))

distanceToCourse p course = let (lb, rb) = boundaries course
                            in min (distanceToPolyline p lb) (distanceToPolyline p rb)
