module Main where
import Data.Bool

data Point t = Point { p_x :: t, p_y :: t } deriving (Show)
data Segment t = Segment (Point t) (Point t) deriving (Show)
data Line t = Line { slope :: t,  intercept :: t } deriving (Show)
data Vector t = Vector { v_x :: t, v_y :: t } deriving (Show)
type Position = Point


in_domain x_value (Segment from to) = ((x_value < (p_x from)) && (x_value > (p_x to))) || ((x_value > (p_x from)) && (x_value < (p_x to)))

in_domains :: (Ord t) => t -> [Segment t] -> Bool
in_domains point segments = all (in_domain point) segments

segment_slope :: (Fractional t) => Segment t -> t
segment_slope (Segment from to) =
  let rise = (p_y to) - (p_y from)
      run = (p_x to) - (p_x from)
  in (rise / run)

y_intercept slope (Point x y) = y - (slope * x)

make_line :: (Fractional t) => (Segment t) -> (Line t)
make_line segment = let theslope = segment_slope segment
                        (Segment point _) = segment
                        intercept = y_intercept theslope point
                    in (Line theslope intercept)

value_at line x_value = let (Line slope intercept) = line
                        in (slope * x_value) + intercept

intersection :: (Fractional t) => Line t -> Line t -> Point t
intersection firstLine secondLine =
  let lhsCoefficient = (slope firstLine) - (slope secondLine)
      rhs = (intercept secondLine) - (intercept firstLine)
      x_intersection = rhs / lhsCoefficient
      y_intersection = value_at firstLine x_intersection
  in Point x_intersection y_intersection

segment_intersection :: (Fractional t, Ord t) => Segment t -> Segment t -> Maybe (Point t)
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
 in do
   putStrLn $ show diagonal_segment
   putStrLn $ show $ segment_slope diagonal_segment
   putStrLn $ show $ y_intercept (segment_slope diagonal_segment) first
   putStrLn $ show $ make_line diagonal_segment
   putStrLn $ show flatLine
   putStrLn $ show $ intersection extendedSegment flatLine
   putStrLn $ show $ segment_intersection diagonal_segment flat_segment
   putStrLn $ show $ segment_intersection (Segment (Point 5 0) (Point 10 10))  flat_segment
