module Car where

import GameTypes
import Geometry
import Course

lastSegmentTravelled carState = makeSegment (position carState) (priorPosition carState)

takeCarTurn :: CarState -> Direction -> CarState
takeCarTurn state direction = coast (accelerate state direction)

coast car = let new_position = translate (position car) (velocity car)
            in CarState new_position (velocity car) (position car)


accelerate car direction = car {velocity = (translate (vectorForDirection direction) (velocity car))}


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

