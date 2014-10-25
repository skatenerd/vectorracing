module Main where
import Data.Bool

data Point = Point { p_x :: Float, p_y :: Float } deriving (Show)
data Vector = Vector { v_x :: Float, v_y :: Float } deriving (Show)
data Segment = Segment Point Point deriving (Show)
data Line = Line { slope :: Float,  intercept :: Float } deriving (Show)
data Range = Range { r_lower :: Float, r_upper :: Float }
data CarState = CarState { position :: Point, velocity :: Vector} deriving (Show)
data Direction = Up | UpRight | UpLeft | LLeft | RRight | DownRight | DownLeft | Down
--data GameState = GameState [CarState]

add :: Vector -> Vector -> Vector
add v1 v2 = Vector ((v_x v1) + (v_x v2)) ((v_y v1) + (v_y v2))

translate p v = Point (v_x v + p_x p) (v_y v + p_y p)

coast car = let new_position = translate (position car) (velocity car)
            in CarState new_position (velocity car)

vector_for_direction :: Direction -> Vector
vector_for_direction Up = Vector 1 0
vector_for_direction UpRight = Vector 1 1
vector_for_direction UpLeft = Vector (1) (-1)
vector_for_direction LLeft = Vector 0 (-1)
vector_for_direction RRight = Vector 0 1
vector_for_direction DownRight = Vector (-1) (1)
vector_for_direction DownLeft = Vector (-1) (-1)
vector_for_direction Down = Vector (-1) (0)

accelerate car direction = CarState (position car) (add (vector_for_direction direction) (velocity car))

take_turn state direction = let with_new_velocity = accelerate state direction
                            in Just $ coast with_new_velocity

in_range value range =
   (value < (r_upper range)) && (value > (r_lower range))

make_range :: Float -> Float -> Range
make_range first second = if (first < second) then (Range first second) else (Range second first)

in_domain x_value (Segment from to) = in_range x_value $ make_range (p_x from) (p_x to)

in_domains :: Float -> [Segment] -> Bool
in_domains point segments = all (in_domain point) segments

segment_slope :: Segment -> Float
segment_slope (Segment from to) =
  let rise = (p_y to) - (p_y from)
      run = (p_x to) - (p_x from)
  in (rise / run)

y_intercept slope (Point x y) = y - (slope * x)

make_line :: Segment -> Line
make_line segment = let theslope = segment_slope segment
                        (Segment point _) = segment
                        intercept = y_intercept theslope point
                    in (Line theslope intercept)

value_at line x_value = let (Line slope intercept) = line
                        in (slope * x_value) + intercept

intersection :: Line -> Line -> Point
intersection firstLine secondLine =
  let lhsCoefficient = (slope firstLine) - (slope secondLine)
      rhs = (intercept secondLine) - (intercept firstLine)
      x_intersection = rhs / lhsCoefficient
      y_intersection = value_at firstLine x_intersection
  in Point x_intersection y_intersection

segment_intersection :: Segment -> Segment -> Maybe (Point)
segment_intersection first_segment second_segment =
  let first_line = make_line first_segment
      second_line = make_line second_segment
      the_intersection = intersection first_line second_line
      legal = in_domains (p_x the_intersection) [first_segment, second_segment]
  in if legal
     then Just the_intersection
     else Nothing

main = let first = Point 2 0
           second = Point 3 2.5
           diagonal_segment = Segment first second
           extendedSegment = make_line diagonal_segment
           flat_segment = Segment (Point 5 5) (Point 10 5)
           flatLine = make_line $ flat_segment
           vertical_line = make_line $ Segment (Point 2 0) (Point 2 10)
           initial_car_state = CarState (Point 0 0) (Vector 0 0)
           Just after_right = take_turn initial_car_state RRight
           Just after_right_up = take_turn after_right Up
           Just after_right_up_up = take_turn after_right_up Up
           Just after_right_up_up_up = take_turn after_right_up_up Up
           far_up_little_right = take_turn after_right_up_up_up Up

 in do
   putStrLn $ show diagonal_segment
   putStrLn $ show $ segment_slope diagonal_segment
   putStrLn $ show $ y_intercept (segment_slope diagonal_segment) first
   putStrLn $ show $ make_line diagonal_segment
   putStrLn $ show flatLine
   putStrLn $ show vertical_line
   putStrLn $ show $ intersection extendedSegment flatLine
   putStrLn $ show $ segment_intersection diagonal_segment flat_segment
   putStrLn $ show $ segment_intersection (Segment (Point 5 0) (Point 10 10))  flat_segment
   putStrLn $ show $ far_up_little_right
