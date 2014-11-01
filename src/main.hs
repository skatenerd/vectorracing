module Main where
import Data.Bool

data Point = Point { p_x :: Float, p_y :: Float } deriving (Show)
data Vector = Vector { v_x :: Float, v_y :: Float } deriving (Show)
data Segment = Segment Point Point deriving (Show)

-- FunctionLine = slope, intercept
-- VerticalLine = intercept
data Line = FunctionLine Float Float | VerticalLine Float deriving (Show)

data Range = Range { r_lower :: Float, r_upper :: Float }
data CarState = CarState { position :: Point, velocity :: Vector} deriving (Show)
data Direction = Up | UpRight | UpLeft | LLeft | RRight | DownRight | DownLeft | Down
type Shape = [Point]
type Course = [Shape]
--data GameState = GameState [CarState]

add :: Vector -> Vector -> Vector
add v1 v2 = Vector ((v_x v1) + (v_x v2)) ((v_y v1) + (v_y v2))

translate p v = Point (v_x v + p_x p) (v_y v + p_y p)

coast car = let new_position = translate (position car) (velocity car)
            in CarState new_position (velocity car)

vector_for_direction :: Direction -> Vector
vector_for_direction Up = Vector 0 1
vector_for_direction UpRight = Vector 1 1
vector_for_direction UpLeft = Vector (-1) (1)
vector_for_direction LLeft = Vector (-1) 0
vector_for_direction RRight = Vector 1 0
vector_for_direction DownRight = Vector (1) (-1)
vector_for_direction DownLeft = Vector (-1) (-1)
vector_for_direction Down = Vector (0) (-1)

hits_shape segment shape = let walls = zipWith make_segment shape ((tail shape) ++ [shape !! 0])
                           in any (segment_intersects segment) walls

hits_course segment course = any (hits_shape segment) course

accelerate car direction = CarState (position car) (add (vector_for_direction direction) (velocity car))

take_turn state direction course = let with_new_velocity = accelerate state direction
                                       after_coast = coast with_new_velocity
                                       segment_created = make_segment (position state) (position after_coast)
                                   in if hits_course segment_created course then
                                         Nothing else
                                         Just after_coast

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

make_segment from to = Segment from to

is_vertical (Segment from to) = (p_x from) == (p_x to)

make_line :: Segment -> Line
make_line segment = if is_vertical segment
                    then let Segment (Point x _) _ = segment
                         in VerticalLine x
                    else let theslope = segment_slope segment
                             (Segment point _) = segment
                             intercept = y_intercept theslope point
                         in (FunctionLine theslope intercept)

-- Warning! LOL! Vertical lines aren't functions, so it's hard to talk about them in a functional language!!
value_at (VerticalLine intercept) _ = intercept
value_at (FunctionLine slope intercept) x_value = (slope * x_value) + intercept

intersection :: Line -> Line -> Maybe Point
intersection (VerticalLine _) (VerticalLine _) = Nothing
intersection (FunctionLine slope intercept) (VerticalLine i) = Just $ Point i (value_at (FunctionLine slope intercept) i)
intersection (VerticalLine i) (FunctionLine slope intercept) = Just $ Point i (value_at (FunctionLine slope intercept) i)
intersection (FunctionLine firstSlope firstIntercept) (FunctionLine secondSlope secondIntercept) =
  if (firstSlope == secondSlope)
  then Nothing
  else let lhsCoefficient = firstSlope - secondSlope
           rhs = secondIntercept - firstIntercept
           x_intersection = rhs / lhsCoefficient
           y_intersection = value_at (FunctionLine firstSlope firstIntercept) x_intersection
  in Just $ Point x_intersection y_intersection

segment_intersection :: Segment -> Segment -> Maybe (Point)
segment_intersection first_segment second_segment =
  let first_line = make_line first_segment
      second_line = make_line second_segment
      the_intersection = intersection first_line second_line
  in case the_intersection of Nothing -> Nothing
                              Just (Point p_x p_y) -> let legal = in_domains p_x [first_segment, second_segment]
                                                      in if legal
                                                      then the_intersection
                                                      else Nothing

segment_intersects :: Segment -> Segment -> Bool
segment_intersects first second = let intersection = (segment_intersection first second)
                                  in case intersection of Nothing -> False
                                                          (Just _) -> True

main = let first = Point 2 0
           second = Point 3 2.5
           diagonal_segment = Segment first second
           extendedSegment = make_line diagonal_segment
           flat_segment = Segment (Point 5 5) (Point 10 5)
           flatLine = make_line $ flat_segment
           vertical_line = make_line $ Segment (Point 2 0) (Point 2 10)
           square_barrier = [(Point (-8) (-8)), (Point (-8) 8), (Point 8 8), (Point 8 (-8))]
           square_course = [square_barrier]
           initial_car_state = CarState (Point 0 0) (Vector 0 0)
           Just after_right = take_turn initial_car_state RRight square_course
           Just after_right_up = take_turn after_right Up square_course
           Just after_right_up_up = take_turn after_right_up Up square_course
           Just after_right_up_up_up = take_turn after_right_up_up Up square_course
           off_the_end = take_turn after_right_up_up_up Up square_course
           walls = zipWith make_segment square_barrier ((tail square_barrier) ++ [square_barrier !! 0])
 in do
   putStrLn $ show diagonal_segment
   putStrLn $ show $ segment_slope diagonal_segment
   putStrLn $ show $ y_intercept (segment_slope diagonal_segment) first
   putStrLn $ show $ make_line diagonal_segment
   putStrLn $ show flatLine
   putStrLn $ show vertical_line
   --putStrLn $ show $ intersection extendedSegment flatLine
   --putStrLn $ show $ segment_intersection diagonal_segment flat_segment
   --putStrLn $ show $ segment_intersection (Segment (Point 5 0) (Point 10 10))  flat_segment
   putStrLn $ show $ after_right_up_up_up
   putStrLn $ show $ off_the_end
   --putStrLn $ show $ zipWith make_segment square_barrier ((tail square_barrier) ++ [square_barrier !! 0])
   --putStrLn $ show $ hits_shape (Segment (Point 0 0) (Point 1 101)) square_barrier
   putStrLn $ show $ segment_intersection (Segment (Point 0 0) (Point 0 101)) (Segment (Point (-5) 10) (Point 5 10))
   putStrLn $ show $ segment_intersection (Segment (Point (-5) 10) (Point 5 10)) (Segment (Point 0 0) (Point 0 101))

   --putStrLn $ show $ (walls !! 1)
