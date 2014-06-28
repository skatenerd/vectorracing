module Main where

data Point t = Point t t deriving (Show)
data Segment t = Segment (Point t) (Point t) deriving (Show)
data Line t = Line t t deriving (Show)

slope :: (Fractional t) => Segment t -> t
slope (Segment (Point x1 y1) (Point x2 y2)) =
  let rise = y2 - y1
      run = x2 - x1
  in (rise / run)

y_intercept slope (Point x y) = y - (slope * x)

make_line :: (Fractional t) => (Segment t) -> (Line t)
make_line segment = let theslope = slope segment
                        (Segment point _) = segment
                        intercept = y_intercept theslope point
                    in (Line theslope intercept)

value_at line x_value = let (Line slope intercept) = line
                        in (slope * x_value) + intercept

intersection :: (Fractional t) => Line t -> Line t -> Point t
intersection (Line firstSlope firstIntercept) (Line secondSlope secondIntercept) =
  let lhsCoefficient = firstSlope - secondSlope
      rhs = secondIntercept - firstIntercept
      x_intersection = rhs / lhsCoefficient
      y_intersection = value_at (Line firstSlope firstIntercept) x_intersection
  in Point x_intersection y_intersection


main = let first = Point 2 0
           second = Point 3 2.5
           segment = Segment first second
           extendedSegment = make_line segment
           flatLine = make_line $ Segment (Point 5 5) (Point 10 5)
 in do
   putStrLn $ show segment
   putStrLn $ show $ slope segment
   putStrLn $ show $ y_intercept (slope segment) first
   putStrLn $ show $ make_line segment
   putStrLn $ show flatLine
   putStrLn $ show $ intersection extendedSegment flatLine
