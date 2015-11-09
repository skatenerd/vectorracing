module Ai(getMove, progress) where

import Course
import GameTypes
import Geometry
import Car
import Scoring

import Data.Maybe
import Data.List.Extras

-- API
getMove course carState depth = extractMove $ bestFutureState course carState depth
                                where extractMove (Just node) = let InfiniTree (_, history) _ = node
                                                                    (direction, _) = head history
                                                                in direction
                                      extractMove Nothing = Up


-- tree-representation for reasoning-about-the-future
data InfiniTree a = InfiniTree { getValue :: a, getChildren :: [InfiniTree a] } deriving (Show)

-- top-level algorithm
bestFutureState course state searchDepth = bestNodeAtDepth searchDepth (scoreState course) pruner (makeFutureTree state [])
  where pruner node = let InfiniTree (carState, history) _ = node
                      in not ((willCrash node) || (goingBackwards state node))
        goingBackwards seedState candidate = dot forwardsAtCar (velocity carState) < 0
            where InfiniTree (carState, history) _ = candidate
                  forwardsDirectionWaypoints = makeSegments $ pointsAlong course
                  closestWaypoint = argmin distanceFromCar forwardsDirectionWaypoints
                  distanceFromCar (Segment start end) = distance (position carState) start
                  forwardsAtCar = segmentToVector closestWaypoint
        -- Can I stop in time to not crash?
        willCrash node = let  InfiniTree (state, _) _ = node
                              projectedState = coast state
                              projectedSegment = scaleSegment (Segment (priorPosition projectedState) (position projectedState)) ((vnorm (velocity state)) / 2)
                              (leftBoundaries, rightBoundaries) = getBoundaries course
                          in (hitsPolyline projectedSegment leftBoundaries) || (hitsPolyline projectedSegment rightBoundaries)


-- the top-level algorithm says we should search the tree.  How do we search the tree? we recursively...look at it.
bestNodeAtDepth 0 _ _ node = return node
bestNodeAtDepth depth score prune (InfiniTree value children) = let searchableChildren = (filter prune children)
                                                                    bestNodesUnderChildren = (map (bestNodeAtDepth (depth - 1) score prune) searchableChildren)
                                                                    winner = safeArgmax score bestNodesUnderChildren
                                                                    safeArgmax predicate items = (catchNull $ argmax predicate) items
                                                                in  fromMaybe Nothing winner

-- if we are going to traverse a tree, first we have to build it
-- the data structure is a little gross.  history should get simplified to not be a tuple.
-- perhaps we should absorb the "history" into the "history" concept residing on CarState...
makeFutureTree state pathToHere = InfiniTree (state, pathToHere) (theSubtrees state pathToHere)
theSubtrees state pathToHere = let makeForDirection d = makeFutureTree (takeCarTurn state d) (pathToHere ++ [(d, state)])
                               in map makeForDirection [Up, Down, LLeft, RRight]


-- also, we need our scoring function
scoreState course Nothing = 0
scoreState course (Just node) = let InfiniTree (state, howigothere) _ = node
                                    barsCrossed = progress course ((map (position . snd) howigothere) ++ [position state])
                                in barsCrossed
