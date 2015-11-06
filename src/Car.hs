module Car where

import GameTypes
import Geometry
import Course

lastSegmentTravelled carState = makeSegment (position carState) (priorPosition carState)

addToHistory carState point = CarState $ (positionHistory carState) ++ [point]

takeCarTurn :: CarState -> Direction -> CarState
takeCarTurn state direction = let newVelocity = translate (velocity state) (vectorForDirection direction)
                                  nextPosition = translate (position state) newVelocity
                              in addToHistory state nextPosition

coast car = let newPosition = translate (position car) (velocity car)
            in addToHistory car newPosition

vectorForDirection :: Direction -> Vector
vectorForDirection Up = Vector 0 1
vectorForDirection UpRight = Vector 1 1
vectorForDirection UpLeft = Vector (-1) 1
vectorForDirection LLeft = Vector (-1) 0
vectorForDirection RRight = Vector 1 0
vectorForDirection DownRight = Vector 1 (-1)
vectorForDirection DownLeft = Vector (-1) (-1)
vectorForDirection Down = Vector 0 (-1)


hitsCourse segment course = let (lb, rb) = getBoundaries course
                            in (hitsPolyline segment lb) || (hitsPolyline segment rb)


position car = last (positionHistory car)
priorPosition car = (reverse $ positionHistory car) !! 1
velocity car = pointDifference (position car) (priorPosition car)
--position :: Point, velocity :: Vector, priorPosition :: Point
