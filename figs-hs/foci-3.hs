import Eemian
import System.Random
import Data.List (sortOn)
import Data.List.Split (chunksOf)
import Data.List (sortOn,zip4,intercalate)
import Text.Printf (printf)

-- https://en.wikipedia.org/wiki/B%C3%A9zier_curve
bezier p0 p1 p2 p3 t = foldr1 move0 [
  ((1 - t) ** 3) `scale0` p0,
  (3 * (1 - t) ** 2 * t) `scale0` p1,
  (3 * (1 - t) * t ** 2) `scale0` p2,
  (t ** 3) `scale0` p3 ]


sheepHead1 = [mkPoint (bezier p0 p1 p2 p3 t) 
  | (p0,p1,p2,p3) <- headPts1, t <- [0.0,0.1..0.9]]

headPts1 = zip4 sc1 ex1 ex2 (tail sc1)
  where
   ex2 = [move0 a b | (a,b) <- (zip sc1 ext2)]
   ex1 = [move0 a b | (a,b) <- (zip sc1 ext1)]
   ext2 = [p1 !! 1 | p1 <- pts2]
   ext1 = [p1 !! 0 | p1 <- pts2]
   sc1 = scanl move0 (30,15) pts3
   pts3 = [p1 !! 2 | p1 <- pts2]
   pts2 = chunksOf 3 pts0

sheepHead2 = [Filled (Polygon sheepHeadB)]
sheepHeadB = pts3
  where
    pts3 = map (addCoords (Point 3.38 0.78)) pts2
    pts2 = map (scaleCoords 0.038) sheepHead1


pts0 = [
 (-12.75,26.54), (-35.17,37.72), (-55.08,28.87), 
 (-24.64,-13.44), (-23.61,-46.86), (-14.71,-65.56),
 (11.01,-22.87), (46.80,-38.26), (69.04,-24.41),
 (16.65,14.17), (10.60,40.59), (0.74,61.10) ]

head1 = [Filled (Circle 1.6 (Point 3.2 0.5))] 
legs2 = map PolyLine legPts1

--fociPts = fociOfEllipse 3 2

fociOfEllipse a b = [Point c 0, Point (-c) 0]
  where
    c = sqrt (a * a - b * b)

minY pts = head srt 
  where
    srt = sortOn coordY pts
    coordY (Point x y) = y

maxY pts = head srt 
  where
    srt = sortOn coordY pts
    coordY (Point x y) = -y

cli p1 p2 = (minY . concat) 
  [circleLineIntersections c p1 p2 | c <- circles1]

legPts1 = 
  [[cli p1 p2,p2] 
  | (p1,p2) <- zip p1s p2s]
  where
    p2s = [towards (GON 300) p 3.5 | p <- p1s]
    p1s = [towards a1 p r | p <- pts] ++
          [towards a2 p r | p <- pts]
    a1 = GON 50
    a2 = GON 250
    r = 0.4
    pts = [Point 1.8 0,Point (-1.8) 0]

pointOfEllipse a b t = Point x y 
  where
    x = a * cos t
    y = b * sin t

arcs1 = [toArcLine ([Line p1 p1] ++ arcs)]
  where
    p1 = towards a1 p r
    Arc r p a1 a2 = head arcs
    arcs = [validateArc a | a <- arcs2]

-- | If a2 < a1 then add DEG 360 to a2  
validateArc (Arc r p a1 a2)
  | a1 <= a2  = Arc r p a1 a2
  | otherwise = Arc r p a1 (a2 `add` (RAD twopi))

-- | Arc Double Point Angle Angle
arcs2 = [
  Arc 
  (dist p0 p1)
  p0
  (angleBt (eastVector p0) (mkVector p0 p1))
  (angleBt (eastVector p0) (mkVector p0 p2))
  | (Circle r p0,p1,p2) <- zip3 circles1 dots3 dots2]
  where
    dots3 = [last dots2] ++ dots2

circles1 = [Circle (0.8+rand) p | (p,rand) <- zip pts rands]
  where
    pts = [alongPL pl1 (s * l2) | s <- [1..n]]
    l2 = l1 / n
    l1 = lengthPL pl1
    rands = randomRs (-0.15,0.15) g
    g = mkStdGen 42
    n = 15

-- dots2 = [Filled (Circle 0.15 p) | p <- outer ++ [center]]
dots2 = outer
  where
    outer = [maxDist center p1 p2 | [p1,p2] <- xs]
    xs = [circleCircleIntersections c d 
      | (c,d) <- zip c1 (tail c1)]
    c1 = circles1 ++ [head circles1]

center = Point 0 0

maxDist c a b =
  if dist c a >= dist c b then a else b

pl1 = PolyLine pts

pts = [pointOfEllipse 3 2 t | t <- [0,dt..twopi]]
  where
    dt = twopi / n
    n = 120

ay1 = Point 0 (-2)
ay2 = Point 0 5
ax1 = Point (-2) 0
ax2 = Point 6 0

axes = 
  [Arrow "" ax1 ax2] ++
  [TextE ay2 "$y$" "below right"] ++
  [Arrow "" ay1 ay2] ++
  [TextE ax2 "$x$" "above left"] 

layers1 = [
  Phantom (Point (-10) (-1)) (Point 10 5),
  --Layer "1" Black arcs1 "[line width=0.8pt]",
  Layer "1" Gray30 sheepHead2 "[line width=0.8pt]",
  --Layer "1" Gray30 head1 "[line width=3.2pt]",
  Layer "1" Black legs2 "[line width=3.2pt]",
  Layer "1" Black arcs1 "[line width=2.4pt]"
  ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict


