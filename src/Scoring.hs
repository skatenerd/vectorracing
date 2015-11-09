module Scoring (progress, humanWon, aiWon, someoneWon) where

import Course
import GameTypes
import Geometry

progress course pastPositions = let triplines = concat $ replicate 2 (progressMarkers course)
                                    freshCourse = dropWhile (not . (carHasCrossed pastPositions)) triplines
                                    progressMarkers course = let lrps = getLeftrightPairs course
                                                             in map (uncurry Segment) lrps
                                    carHasCrossed history segment = let segmentHistory = makeSegments history
                                                                    in any (segmentIntersectsGenerous segment) segmentHistory
                                in length $ takeWhile (carHasCrossed pastPositions) freshCourse


playerWon course playerState = totalProgress >= (length $ pointsAlong course)
                               where totalProgress = progress course (positionHistory playerState)
someoneWon course gameState = playerWon course (aiState gameState) || playerWon course (humanState gameState)
aiWon course gameState = playerWon course (aiState gameState)
humanWon course gameState = playerWon course (humanState gameState)

