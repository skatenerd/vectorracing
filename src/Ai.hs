module Ai(getMove) where

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

goingBackwards course seedState candidate = if (crossesStartLine)
                                            then progressAtEnd > progressAtStart
                                            else progressAtEnd < progressAtStart
                                            where InfiniTree (carState, history) _ = candidate
                                                  progressAtStart = argminIndex (distance (position seedState)) waypoints
                                                  progressAtEnd  = argminIndex (distance (position carState)) waypoints
                                                  waypoints = pointsAlong course
                                                  argminIndex score elements = fromMaybe 0 $ findIndex (== (argmin score elements)) elements
                                                  maxV = maximum $ map vnorm (velocity seedState : (velocity carState : (map (velocity . snd) history)))
                                                  crossesStartLine = fromIntegral (abs progressAtStart - progressAtEnd) > (maxV * (fromIntegral $ length history))

-- top-level algorithm
searchDepth = 4
bestFutureState course state = bestNodeAtDepth searchDepth (scoreState course) pruner (makeFutureTree state [])
                               where --fOr = liftA2 (||)
                                     pruner node = let InfiniTree (carState, history) _ = node
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
                                    barsCrossed = progress course state howigothere
                                in barsCrossed


progress course state history = let pastPositions = (map (position . snd) history) ++ [position state]
                                    triplines = concatMap id $ replicate 2 (progressMarkers course)
                                    freshCourse = dropWhile (not . (carHasCrossed pastPositions)) triplines
                                in length $ takeWhile (carHasCrossed pastPositions) freshCourse

progressMarkers course = let lrps = getLeftrightPairs course
                         in map (uncurry Segment) lrps

indexOfLastElement elements predicate = let maybeDistanceFromEnd = ((flip findIndex) . reverse) elements predicate
                                            distanceFromEnd = fromMaybe (length elements) maybeDistanceFromEnd
                                        in (length elements) - distanceFromEnd

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
                             projectedSegment = scaleSegment (Segment (priorPosition projectedState) (position projectedState)) (vnorm (velocity state))
                             (leftBoundaries, rightBoundaries) = boundaries course
                         in (hitsPolyline projectedSegment leftBoundaries) || (hitsPolyline projectedSegment rightBoundaries)

