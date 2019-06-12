
{-|
Module      : Shapes
Description : Basic geometric structures
Stability   : experimental

The module Shapes contains some basic geometric structures 
like points, lines and circles.
-}
module Shapes where

-- | `Point x y` is a representation of a point /(x,y)/ in two dimensions.
-- 
-- >>> Point 2 3
-- Point 2.0 3.0
--
data Point = Point Double Double
  deriving Show

data AngleType a = RAD a | DEG a | GON a 
  deriving Show

type Angle = AngleType Double

data Vector = Vector Double Double
  deriving Show

halfpi = pi / 2
twopi  = 2 * pi

degrees (RAD r) = DEG (r * 180 / pi)
degrees (GON g) = DEG (g * 180 / 200)
degrees (DEG d) = DEG d
gons (RAD r) = GON (r * 200 / pi)
gons (DEG g) = GON (g * 200 / 180)
gons (GON g) = GON g
radians (GON g) = RAD (g * pi / 200)
radians (DEG d) = RAD (d * pi / 180)
radians (RAD r) = RAD r

tan1 (RAD r) = tan r
tan1 r = tan1 (radians (r))

cos1 (RAD r) = cos r
cos1 r = cos1 (radians (r))

sin1 (RAD r) = sin r
sin1 r = sin1 (radians (r))

add (DEG a) (DEG b) = DEG (a + b)
add (RAD a) (RAD b) = RAD (a + b)
add (GON a) (GON b) = GON (a + b)
add (DEG a) (RAD b) = radians (DEG a) `add` (RAD b)
add (RAD a) (DEG b) = (RAD a) `add` radians (DEG b)
add (GON a) (RAD b) = radians (GON a) `add` (RAD b)
add (RAD a) (GON b) = (RAD a) `add` radians (GON b)

sub (DEG a) (DEG b) = DEG (a - b)
sub (RAD a) (RAD b) = RAD (a - b)
sub (GON a) (GON b) = GON (a - b)
sub (DEG a) (RAD b) = radians (DEG a) `sub` (RAD b)
sub (RAD a) (DEG b) = (RAD a) `sub` radians (DEG b)
sub (GON a) (RAD b) = radians (GON a) `sub` (RAD b)
sub (RAD a) (GON b) = (RAD a) `sub` radians (GON b)

axisX = Vector 1.0 0.0
axisY = Vector 0.0 1.0

mkVector (Point x0 y0) (Point x1 y1) =
  Vector (x1 - x0) (y1 - y0)

angleBt (Vector x1 y1) (Vector x2 y2) = RAD t
  where
    t = atan2 (x1*y2 - y1*x2) (x1*x2 + y1*y2)

eastVector (Point x y) = mkVector 
  (Point x y) (Point (x+1) y)

intToDouble :: Int -> Double
intToDouble i = fromRational (toRational i)

avg xs = sum xs / n
  where
    n = intToDouble (length xs)

-- | A `Shape` can be a `Circle` or a `Line`.
--
-- >>> p1 = Point 1 1
-- 
-- >>> p2 = Point 3 2
-- 
-- >>> Line p1 p2
-- Line (Point 1.0 1.0) (Point 3.0 2.0)
-- 
-- >>> Circle 0.5 (Point 2 3)
-- Circle 0.5 (Point 2.0 3.0)
--
data Shape = Circle Double Point
  | Line Point Point
  | PolyLine [Point]
  | Arc Double Point Angle Angle
  deriving Show

pointOfEllipse a b t = Point x y 
  where
    x = a * cos t
    y = b * sin t


-- | Intersection points of a circle and a line
-- circle = Circle r (Point x y)
-- line = Line (Point x1 y1) (Point x2 y2)
circleLineIntersections circle (Point x1 y1) (Point x2 y2) = pts2
  where
    Circle r (Point x0 y0) = circle
    pts2 = [Point (x1+x0) (y1+y0) | Point x1 y1 <- pts1]
    pts1 = circleLineIntersections1 r 
      (Point (x1-x0) (y1-y0)) 
      (Point (x2-x0) (y2-y0))

-- | Intersection points of a circle at origo and a line.
-- circle = Circle r (Point 0 0)
-- line = Line (Point x1 y1) (Point x2 y2)
-- Algorithm from
-- mathworld.wolfram.com/Circle-LineIntersection.html 
circleLineIntersections1 r (Point x1 y1) (Point x2 y2)
  | discr < 0  = []
  | discr == 0 = [Point x3 y3]
  | discr > 0  = [Point x3 y3, Point x4 y4]
  where
    sqr x = x * x
    dx = x2 - x1
    dy = y2 - y1
    dr = sqrt ((sqr dx) + (sqr dy))
    det = x1 * y2 - x2 * y1
    sign x 
      | x < 0  = (-1)
      | otherwise = 1
    discr = sqr r * sqr dr - sqr det
    x3 = (det * dy + sign dy * dx * sqrt discr) / (sqr dr)
    y3 = ((-det) * dx + abs dy * sqrt discr) / (sqr dr)
    x4 = (det * dy - sign dy * dx * sqrt discr) / (sqr dr)
    y4 = ((-det) * dx - abs dy * sqrt discr) / (sqr dr)



-- | Intersection points of two circles
-- Algorithm from
-- http://paulbourke.net/geometry/circlesphere/
circleCircleIntersections circle1 circle2 
  | d > r1 + r2        = [] -- none
  | d < abs (r1 - r2)  = [] -- none
  | d == 0 && r1 == r2 = [] -- infinitely many
  | otherwise  = [Point x3 y3, Point x4 y4]
  where
    h = sqrt((r1*r1) - (a*a))
    a = (r1*r1 -  r2*r2 + d*d) / (2*d)
    d = dist p1 p2
    Point x1 y1 = p1
    Point x2 y2 = p2
    Circle r1 p1 = circle1
    Circle r2 p2 = circle2
    (dx,dy) = (x2-x1,y2-y1)
    x = x1 + a * dx / d   
    y = y1 + a * dy / d   
    [x3,x4] = [x `mp` (h * dy / d) | mp <- [(-),(+)]]
    [y3,y4] = [y `pm` (h * dx / d) | pm <- [(+),(-)]]

-- | A point with distance d along a PolyLine pl
alongPL pl d = along1 0 d pts
  where
    PolyLine pts = pl

-- | Recursive algorithm (internal)
along1 done left rest 
  | length rest == 1 = head rest
  | left > d1 = along1 (done + d1) (left - d1) (tail rest)
  | otherwise = towards1 p1 p2 (left / d1)
  where
    d1 = dist p1 p2
    p1 = head rest
    p2 = head (tail rest)

-- | From point p1 towards p2 with respect to ratio
towards1 p1 p2 ratio = Point (x1 + r * x3) (y1 + r * y3)
  where
    (x3,y3) = (x2 - x1,y2 - y1)
    r = ratio `min` 1.0
    Point x1 y1 = p1
    Point x2 y2 = p2

-- | Length of PolyLine
lengthPL pl = sum [dist p1 p2
  | (p1,p2) <- zip pts (tail pts)]
  where
    PolyLine pts = pl

-- | The euclidian distance between two points.
dist (Point x0 y0) (Point x1 y1) = 
  sqrt ((sqr dx) + (sqr dy))
  where
    sqr x = x * x
    dx = x1 - x0
    dy = y1 - y0



