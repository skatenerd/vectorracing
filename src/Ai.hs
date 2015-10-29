module Ai(InfiniTree(InfiniTree), bestFutureState) where

import Course
import GameTypes
import Geometry
import Car

import Data.Maybe
import Data.List

progressMarkers course = let lrps = getLeftrightPairs course
                         in map (uncurry Segment) lrps

indexOfLastElement elements predicate = let maybeDistanceFromEnd = ((flip findIndex) . reverse) elements predicate
                                            distanceFromEnd = fromMaybe (length elements) maybeDistanceFromEnd
                                        in (length elements) - distanceFromEnd

progress :: Course -> CarState -> Integer
progress course state = fromIntegral $ indexOfLastElement (progressMarkers course) (carHasCrossed state)

data InfiniTree a = InfiniTree { getValue :: a, getChildren :: [InfiniTree a] } deriving (Show)

theSubtrees state pathToHere = let makeForDirection d = makeFutureTree (takeCarTurn state d) (pathToHere ++ [(d, state)])
                               in map makeForDirection [Up, Down, LLeft, RRight]

makeFutureTree state pathToHere = InfiniTree (state, pathToHere) (theSubtrees state pathToHere)

bestNodeAtDepth 0 _ _ node = return node
bestNodeAtDepth depth score prune (InfiniTree value children) = argmax score (map (bestNodeAtDepth (depth - 1) score prune) (filter prune children))

argmax score elements = if null elements
                        then Nothing
                        else foldr takemax (head elements) elements
                        where takemax a b = if (score a) > (score b)
                                            then a
                                            else b

collidesWithCourse course node = let InfiniTree (_, howigothere) _ = node
                                     mypath = makeSegments $ map (position . snd) howigothere
                                     collision = any ((flip hitsCourse) course) mypath
                                 in collision

bestFutureState course state = bestNodeAtDepth 4 (scoreState course) (not . (collidesWithCourse course)) (makeFutureTree state [])

scoreState course Nothing = 0
scoreState course (Just node) = let InfiniTree (state, _) _ = node
                                    barsCrossed = progress course state
                                in barsCrossed


carHasCrossed state segment = let zoom = lastSegmentTravelled state
                              in segmentIntersects zoom segment


