module Rendering (render, renderInto, courseWidth, courseHeight) where

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
                                           closeToCourse = distanceToCourse thePoint course <= roadWidth

placeFinish x y gameState course = if closeToFinish
                                   then Just ('F', Finish)
                                   else Nothing
                                     where thePoint = ((Point `on` fromIntegral) x y)
                                           closeToFinish = distanceToSegment thePoint finishSegment < 0.5
                                           finishSegment = (uncurry Segment) (last (getLeftrightPairs course))

renderManySquares gameState course squares = fromMaybe ('D', Dust) possibleAnswer
  where possibleAnswer :: Maybe (Char, GameColors)
        possibleAnswer = foldl maybeOr Nothing tries
        tries :: [Maybe (Char, GameColors)]
        tries = map tryForSquare squares
        tryForSquare :: (Integer, Integer) -> Maybe (Char, GameColors)
        tryForSquare (x, y) = maybeRenderSquare gameState course (x,y)

maybeRenderSquare gameState course (x, y) = foldl maybeOr Nothing [placeCar x y gameState course,
                                                                   placeAI x y gameState course,
                                                                   placeWhoosh x y gameState course,
                                                                   placeWall x y gameState course,
                                                                   placeFinish x y gameState course,
                                                                   placeRoad x y gameState course,
                                                                   placeEarth x y gameState course]

-- https://gist.github.com/skatenerd/767e2042f388bde63779
nestedMap = map . map

renderPadding = 1
minX course = floor (minimum $ map pX (allBoundaryPoints course)) - renderPadding
maxX course = ceiling (maximum $ map pX (allBoundaryPoints course)) + renderPadding
minY course = floor (minimum $ map pY (allBoundaryPoints course)) - renderPadding
maxY course = ceiling (maximum $ map pY (allBoundaryPoints course)) + renderPadding

allBoundaryPoints course = uncurry (++) (unzip (getLeftrightPairs course))

courseWidth course = fromIntegral $ (maxX course) - (minX course) + 1
courseHeight course = fromIntegral $ (maxY course) - (minY course) + 1

render :: GameState -> Course -> [(Char, GameColors)]
render state course = renderInto state course (courseWidth course) (courseHeight course)

renderInto :: GameState -> Course -> Integer -> Integer -> [(Char, GameColors)]
renderInto state course renderWidth renderHeight =
                      intercalate [('\n', Dust)] rows
                      where rows :: [[(Char, GameColors)]]
                            rows = nestedMap (renderManySquares state course) cells
                            cells :: [[[(Integer, Integer)]]]
                            cells = nestedMap projectionForCell smallWorldCells
                            smallWorldCells = [zip [0..(renderWidth - 1)] (repeat y) | y <- [renderHeight - 1, renderHeight - 2 .. 0]]
                            projectionForCell (x, y) = [(projectedX, projectedY) | projectedX <- (getXs x), projectedY <- (getYs y)]
                            xScale = max 1 $ (fromIntegral (courseWidth course)) / (fromIntegral renderWidth)
                            yScale =  max 1 $ (fromIntegral (courseHeight course)) / (fromIntegral renderHeight)
                            blowup = (ceiling $ max xScale yScale)
                            getXs x = map (+ (minX course)) [(blowup * x) .. (blowup * x) + (blowup - 1)]
                            getYs y = map (+ (minY course)) [(blowup * y) .. (blowup * y) + (blowup - 1)]
