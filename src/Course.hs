module Course (makeCourse, makeSegments, distanceToCourse, pointsAlong, onRoad, roadWidth) where

import Geometry
import GHC.Float
import GameTypes
import qualified Data.RTree as RT
import Data.List.Extras
import Data.Maybe

makeCourse path obstacles = Course { path = path,
                                     obstacles = obstacles,
                                     getBoundaries = computeBoundaries path,
                                     getLeftrightPairs = computeLeftrightPairs path,
                                     getBoundaryCache = computeBoundaryCache path,
                                     getPathCache = computePathCache path}

computePathCache path =
  let allPathPoints = pointsAlongPath path
  in loadIntoTree allPathPoints

computeBoundaryCache path =
  let (leftBoundaries, righBoundaries) = computeBoundaries path
      boundaryPoints = (concatMap segmentPoints)
      allBoundaryPoints = concatMap boundaryPoints [leftBoundaries, righBoundaries]
  in loadIntoTree allBoundaryPoints

loadIntoTree points =
  let loaded = foldl addToTree RT.empty points
      addToTree have toAdd = RT.insert (RT.mbb x y x y) toAdd have
                             where x = float2Double $ pX toAdd
                                   y = float2Double $ pY toAdd
  in loaded


computeBoundaries path = let leftrightpairs = computeLeftrightPairs path
                             (lefts, rights) = unzip leftrightpairs
                         in (makeSegments lefts, makeSegments rights)

roadWidth = 5


pointsAlong course = pointsAlongPath $ path course
pointsAlongPath path = concatMap segmentPoints (makeSegments path)

polylinePoints polyline = concatMap segmentPoints

makeSegments points = zipWith Segment points (tail points)

computeLeftrightPairs path = concatMap pairsForSegment (makeSegments path)
  where pairsForSegment segment = fmap (makeLeftRightPair segment) (segmentPoints segment)
        makeLeftRightPair segment point = let normal = unitNormal segment
                                              scaled = scale normal roadWidth
                                              inverted = scale scaled (-1)
                                          in (translate point inverted, translate point scaled)


distanceToCourse p course = let (lb, rb) = getBoundaries course
                            in min (distanceToPolyline p lb) (distanceToPolyline p rb)


onRoad p course = let boundingBox = makeBoundingBox (4 * (float2Double roadWidth)) p --gratuitous!
                      closeWallPoints = RT.lookupRange boundingBox (getBoundaryCache course)
                      closePathPoints = RT.lookupRange boundingBox (getPathCache course)
                      closestWallPoint = safeArgmin (distance p) closeWallPoints
                      closestPathPoint = safeArgmin (distance p) closePathPoints
                      onCenterLine = distance p (fromMaybe (Point (1/0) (1/0)) closestPathPoint) < 1
                      checkBetween = do  -- monads!
                        wallPoint <- closestWallPoint
                        roadPoint <- closestPathPoint
                        return $ between p wallPoint roadPoint
                      safeArgmin predicate items = (catchNull $ argmin predicate) items
                  in onCenterLine || fromMaybe False checkBetween
