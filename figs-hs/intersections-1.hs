import Eemian
import System.Random
import Data.List (sortBy)
import Data.Ord (comparing)

--foci1 = [Filled (Circle 0.15 p) | p <- fociPts]

head1 = [Filled (Circle 1.6 (Point 3.2 0.5))] 
legs2 = map PolyLine legPts1

--fociPts = fociOfEllipse 3 2

fociOfEllipse a b = [Point c 0, Point (-c) 0]
  where
    c = sqrt (a * a - b * b)

minY pts = head srt 
  where
    srt = sortBy (comparing ptY) pts
    ptY (Point x y) = y

maxY pts = head srt 
  where
    srt = sortBy (comparing pty) pts
    ptY (Point x y) = -y

cli p1 p2 = (minY . concat) 
  [circleLineIntersections c p1 p2 | c <- circles1]


ics2 = [Filled (Circle 0.15 p) | p <- ics1]

ics1 = ics0 p1 p2 circles1
  where
    p1 = Point 2 0
    p2 = towards (DEG 270) p1 1

ics0 p1 p2 cs = concat
  [circleLineIntersections c p1 p2 | c <- cs]

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

line1 = [Line (Point 2 (-3)) (Point 2 3)]

layers1 = [
  Phantom (Point (-10) (-1)) (Point 10 5),
  Layer "1" Black ics2 "[line width=0.8pt]",
  Layer "1" Black line1 "[line width=0.8pt,dashed]",
  --Layer "1" Gray30 head1 "[line width=0.8pt]",
  Layer "1" Black circles1 "[line width=0.8pt]"
  --Layer "1" Black arcs1 "[line width=0.8pt]"
  ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict


