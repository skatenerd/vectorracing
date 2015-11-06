module Rendering (render) where

import Geometry
import Car
import GameTypes
import Course
import Data.Function
import Data.List
import Data.Maybe

maybeOr a b = case a of
              Nothing -> b
              Just _ -> a

areSamePoint (x,y) p = (coerceMakePoint x y) == p

coerceMakePoint = Point `on` fromIntegral

placeCar x y gameState course = if areSamePoint (x,y) (position $ humanState gameState)
                                then Just ('C', Car)
                                else Nothing

placeWhoosh x y gameState course = let human = humanState gameState
                                       whooshSegment = Segment (position human) (priorPosition human)
                                       candidatePoint = (coerceMakePoint x y)
                                   in if distanceToSegment candidatePoint whooshSegment < 0.5
                                   then Just ('*', Dust)
                                   else Nothing


placeAI x y gameState course = if areSamePoint (x,y) (position $ aiState gameState)
                               then Just ('A', Opponent)
                               else Nothing

placeEarth x y gameState course = Just ('E', Earth)

placeWall x y gameState course = if closeToCourse && (not (onRoad thePoint course))
                                   then Just ('W', Wall)
                                   else Nothing
                                     where closeToCourse = distanceToCourse thePoint course < 2
                                           thePoint = ((Point `on` fromIntegral) x y)

placeRoad x y gameState course = if closeToCourse && (onRoad thePoint course)
                                   then Just ('R', Road)
                                   else Nothing
                                     where thePoint = ((Point `on` fromIntegral) x y)
                                           closeToCourse = distanceToCourse thePoint course <= courseWidth



renderSquare :: Integer -> Integer -> GameState -> Course -> (Char, GameColors)
renderSquare x y gameState course = fromMaybe ('D', Dust) $ foldl maybeOr Nothing [placeCar x y gameState course,
                                                                                   placeAI x y gameState course,
                                                                                   placeWhoosh x y gameState course,
                                                                                   placeWall x y gameState course,
                                                                                   placeRoad x y gameState course,
                                                                                   placeEarth x y gameState course]

renderRow :: Integer -> GameState -> Course -> [(Char, GameColors)]
renderRow y state course = [renderSquare x y state course | x <- [(minX course)..(maxX course)]]

renderPadding = 1
minX course = floor (minimum $ map pX (allBoundaryPoints course)) - renderPadding
maxX course = ceiling (maximum $ map pX (allBoundaryPoints course)) + renderPadding
minY course = floor (minimum $ map pY (allBoundaryPoints course)) - renderPadding
maxY course = ceiling (maximum $ map pY (allBoundaryPoints course)) + renderPadding

allBoundaryPoints course = uncurry (++) (unzip (getLeftrightPairs course))

render :: GameState -> Course -> [(Char, GameColors)]
render state course = intercalate [('\n', Dust)] rows
                      where rows :: [[(Char, GameColors)]]
                            rows = [renderRow y state course | y <- [maxY course, maxY course - 1 .. minY course]]

