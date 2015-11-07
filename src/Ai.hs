module Ai(getMove, progress) where

import Course
import GameTypes
import Debug.Trace
import Geometry
import Car

import Data.Maybe
import Data.List
import Data.List.Extras
import Control.Applicative
-- API
getMove course carState = extractMove $ bestFutureState course carState
                          where extractMove (Just node) = let InfiniTree (_, history) _ = node
                                                              (direction, _) = head history
                                                          in direction
                                extractMove Nothing = Up


-- tree-representation for reasoning-about-the-future
data InfiniTree a = InfiniTree { getValue :: a, getChildren :: [InfiniTree a] } deriving (Show)

-- this is a pretty clownshoes function...
goingBackwards course seedState candidate = dot forwardsAtCar (velocity carState) < 0
    where InfiniTree (carState, history) _ = candidate
          forwardsDirectionWaypoints = makeSegments $ pointsAlong course
          closestWaypoint = argmin distanceFromCar forwardsDirectionWaypoints
          distanceFromCar (Segment start end) = distance (position carState) start
          forwardsAtCar = segmentToVector closestWaypoint

-- top-level algorithm
searchDepth = 5
bestFutureState course state = bestNodeAtDepth searchDepth (scoreState course) pruner (makeFutureTree state [])
                               where pruner node = let InfiniTree (carState, history) _ = node
                                                   in if (map fst history) == [RRight, RRight, RRight]
                                                      then not ((willCrash course node) || (goingBackwards course state node))
                                                      else not ((willCrash course node) || (goingBackwards course state node))

safeArgmax predicate items = (catchNull $ argmax predicate) items

bestNodeAtDepth 0 _ _ node = return node
bestNodeAtDepth depth score prune (InfiniTree value children) = let searchableChildren = (filter prune children)
                                                                    bestNodesUnderChildren = (map (bestNodeAtDepth (depth - 1) score prune) searchableChildren)
                                                                    winner = safeArgmax score bestNodesUnderChildren
                                                                in  fromMaybe Nothing winner

-- if we are going to traverse a tree, first we have to build it

makeFutureTree state pathToHere = InfiniTree (state, pathToHere) (theSubtrees state pathToHere)
theSubtrees state pathToHere = let makeForDirection d = makeFutureTree (takeCarTurn state d) (pathToHere ++ [(d, state)])
                               in map makeForDirection [Up, Down, LLeft, RRight]


-- also, we need our scoring function


scoreState course Nothing = 0
scoreState course (Just node) = let InfiniTree (state, howigothere) _ = node
                                    barsCrossed = progress course ((map (position . snd) howigothere) ++ [position state])
                                in barsCrossed


progress course pastPositions = let triplines = concat $ replicate 2 (progressMarkers course)
                                    freshCourse = dropWhile (not . (carHasCrossed pastPositions)) triplines
                                in length $ takeWhile (carHasCrossed pastPositions) freshCourse

progressMarkers course = let lrps = getLeftrightPairs course
                         in map (uncurry Segment) lrps

carHasCrossed history segment = let segmentHistory = makeSegments history
                                in any (segmentIntersectsGenerous segment) segmentHistory

-- finally, a utility for pruning the tree
collidesWithCourse course node = let InfiniTree (_, howigothere) _ = node
                                     mypath = makeSegments $ map (position . snd) howigothere
                                     collision = hitsCourse (last mypath) course
                                 in if null mypath
                                 then False
                                 else collision


-- Can I stop in time to not crash?
willCrash course node = let  InfiniTree (state, _) _ = node
                             projectedState = coast state
                             projectedSegment = scaleSegment (Segment (priorPosition projectedState) (position projectedState)) ((vnorm (velocity state)) / 2)
                             (leftBoundaries, rightBoundaries) = getBoundaries course
                         in (hitsPolyline projectedSegment leftBoundaries) || (hitsPolyline projectedSegment rightBoundaries)

