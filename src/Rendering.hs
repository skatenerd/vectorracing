module Rendering (render) where

import Geometry
import GameTypes
import Course
import Data.Function
import Data.List
import Data.Maybe

distanceToCourse p course = let (lb, rb) = boundaries course
                            in min (distanceToPolyline p lb) (distanceToPolyline p rb)

maybeOr a b = case a of
              Nothing -> b
              Just _ -> a

areSamePoint (x,y) p = (coerceMakePoint x y) == p

coerceMakePoint = Point `on` fromIntegral

placeCar x y gameState course = if areSamePoint (x,y) (position $ humanState gameState)
                                then Just "C"
                                else Nothing

placeWhoosh x y gameState course = let human = humanState gameState
                                       whooshSegment = Segment (position human) (priorPosition human)
                                       candidatePoint = (coerceMakePoint x y)
                                   in if distanceToSegment candidatePoint whooshSegment < 0.5
                                   then Just "*"
                                   else Nothing


placeAI x y gameState course = if areSamePoint (x,y) (position $ aiState gameState)
                               then Just "A"
                               else Nothing

placeEarth x y gameState course = Just "_"

placeCourse x y gameState course = if (distanceToCourse ((Point `on` fromIntegral) x y) course) < 0.5
                                  then Just "W"
                                  else Nothing

renderSquare :: Integer -> Integer -> GameState -> Course -> String
renderSquare x y gameState course = fromMaybe " " $ foldl maybeOr Nothing [placeCar x y gameState course,
                                                                          placeWhoosh x y gameState course,
                                                                          placeAI x y gameState course,
                                                                          placeCourse x y gameState course,
                                                                          placeEarth x y gameState course]

renderRow :: Integer -> GameState -> Course -> String
renderRow y state course = intercalate "" [renderSquare x y state course | x <- [-20..55]]

render :: GameState -> Course -> String
render state course = intercalate "\n" [renderRow y state course | y <- [15, 14 .. -10]]

