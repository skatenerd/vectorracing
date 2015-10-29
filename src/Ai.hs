module Ai(InfiniTree(InfiniTree), bestFutureState) where

import Course
import GameTypes
import Geometry
import Car

import Data.Maybe
import Data.List


-- tree-representation for reasoning-about-the-future
data InfiniTree a = InfiniTree { getValue :: a, getChildren :: [InfiniTree a] } deriving (Show)

-- top-level algorithm
searchDepth = 4
bestFutureState course state = bestNodeAtDepth searchDepth (scoreState course) (not . (collidesWithCourse course)) (makeFutureTree state [])

bestNodeAtDepth 0 _ _ node = return node
bestNodeAtDepth depth score prune (InfiniTree value children) = argmax score (map (bestNodeAtDepth (depth - 1) score prune) (filter prune children))

argmax score elements = if null elements
                        then Nothing
                        else foldr takemax (head elements) elements
                        where takemax a b = if (score a) > (score b)
                                            then a
                                            else b

-- if we are going to traverse a tree, first we have to build it

makeFutureTree state pathToHere = InfiniTree (state, pathToHere) (theSubtrees state pathToHere)
theSubtrees state pathToHere = let makeForDirection d = makeFutureTree (takeCarTurn state d) (pathToHere ++ [(d, state)])
                               in map makeForDirection [Up, Down, LLeft, RRight]


-- also, we need our scoring function

scoreState course Nothing = 0
scoreState course (Just node) = let InfiniTree (state, _) _ = node
                                    barsCrossed = progress course state
                                in barsCrossed

progress :: Course -> CarState -> Integer
progress course state = fromIntegral $ indexOfLastElement (progressMarkers course) (carHasCrossed state)

progressMarkers course = let lrps = getLeftrightPairs course
                         in map (uncurry Segment) lrps

indexOfLastElement elements predicate = let maybeDistanceFromEnd = ((flip findIndex) . reverse) elements predicate
                                            distanceFromEnd = fromMaybe (length elements) maybeDistanceFromEnd
                                        in (length elements) - distanceFromEnd

carHasCrossed state segment = let zoom = lastSegmentTravelled state
                              in segmentIntersects zoom segment

-- finally, a utility for pruning the tree
collidesWithCourse course node = let InfiniTree (_, howigothere) _ = node
                                     mypath = makeSegments $ map (position . snd) howigothere
                                     collision = any ((flip hitsCourse) course) mypath
                                 in collision


