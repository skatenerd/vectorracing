module Configs(configs, getCourse, getStartAIState, getStartCarState, startStateFromConfig) where

import GameTypes
import Geometry
import Course

data GameConfig = GameConfig { getCourse :: Course, getStartAIState :: CarState, getStartCarState :: CarState }

roundCourseConfig = GameConfig { getCourse = makeCourse [
  (Point 8 0),
  (Point 38 0),
  (Point 46 8),
  (Point 46 18),
  (Point 38 26),
  (Point 8 26),
  (Point 0 18),
  (Point 0 8),
  (Point 7 0),
  (Point 8 0)]
  [],
  getStartCarState = CarState [Point 8 (-2), Point 8 (-2)],
  getStartAIState = CarState [Point 7 2, Point 7 2] }

zigzagCourseConfig = GameConfig { getCourse = makeCourse [(Point (-10) (-10)), (Point 0 0), (Point 10 2), (Point 35 (-5)), (Point 65 12)] [],
                            getStartCarState = CarState [Point (-11) (-11), Point (-11) (-11)],
                            getStartAIState = CarState [Point (-12) (-12), Point (-12) (-12)] }

configs = [roundCourseConfig, zigzagCourseConfig]

startStateFromConfig config = GameState { quitted = False, humanState = (getStartCarState config), aiState = (getStartAIState config) }
