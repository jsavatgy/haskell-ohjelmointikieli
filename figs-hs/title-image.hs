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


sheepEar earPts = [mkPoint (bezier p0 p1 p2 p3 t) 
  | (p0,p1,p2,p3) <- earPts1 earPts, t <- [0.0,0.1..1.0]]

earPts1 earPts = zip4 sc1 ex1 ex2 (tail sc1)
  where
   ex2 = [move0 a b | (a,b) <- (zip sc1 ext2)]
   ex1 = [move0 a b | (a,b) <- (zip sc1 ext1)]
   ext2 = [p1 !! 1 | p1 <- pts2]
   ext1 = [p1 !! 0 | p1 <- pts2]
   sc1 = scanl move0 (0,0) pts3
   pts3 = [p1 !! 2 | p1 <- pts2]
   pts2 = chunksOf 3 pts1
   pts1 = map (\(x,y) -> (x,-y)) earPts

earR = [ 
  (-5.10,-4.20), (-10.63,-1.91), (-11.31,0.94), 
  (-0.54,2.26), (3.02,3.16), (4.79,1.98), 
  (1.97,-1.31), (4.06,-1.91), (5.62,-1.11) ]

earL = [
  (4.44,-4.29), (10.22,-1.96), (10.84,0.11),
  (0.61,2.07), (-0.51,3.04), (-1.88,3.02),
  (-1.37,-0.02), (-5.81,-2.55), (-6.92,-1.44) ]

sheepEars2 = [Filled (Polygon right),
  Filled (Polygon left)]
  where
    right = map (addCoords ptR . scaleCoords 0.2) right0
    left  = map (addCoords ptL . scaleCoords 0.2) left0
    ptR = Point 1.95 1.85
    ptL = Point 3.59 2.26
    right0 = sheepEar earR
    left0 = sheepEar earL



sheepHead1 = [mkPoint (bezier p0 p1 p2 p3 t) 
  | (p0,p1,p2,p3) <- headPts1, t <- [0.0,0.1..0.9]]

headPts1 = zip4 sc1 ex1 ex2 (tail sc1)
  where
   ex2 = [move0 a b | (a,b) <- (zip sc1 ext2)]
   ex1 = [move0 a b | (a,b) <- (zip sc1 ext1)]
   ext2 = [p1 !! 1 | p1 <- pts2]
   ext1 = [p1 !! 0 | p1 <- pts2]
   sc1 = scanl move0 (2,0) pts3
   pts3 = [p1 !! 2 | p1 <- pts2]
   pts2 = chunksOf 3 pts1
   pts1 = map (\(x,y) -> (x,-y)) pts0

--foci1 = [Filled (Circle 0.15 p) | p <- fociPts]

sheepHead2 = [Filled (Polygon sheepHeadB)]
sheepHeadB = pts3
  where
    pts3 = map (addCoords (Point 3.4 0.5)) pts2
    pts2 = map (scaleCoords 0.7) sheepHead1

pts0 = [ 
  (-0.37,-1.28),(-1.09,-2.34),(-1.82,-2.70),
  (-0.50,-0.24),(-1.28,-0.24),(-1.69,0.01),
  (-0.63,0.39),(-1.01,1.22),(-1.00,2.18),
  (0.01,1.31),(0.71,2.29),(1.89,2.68),
  (0.52,0.17),(0.62,0.19),(1.16,0.20),
  (0.40,0.01),(0.56,-0.03),(1.00,-0.29),
  (0.36,-0.22),(0.71,-1.20),(0.45,-2.09) ]

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
  [[addCoords (Point 0 1) (cli p1 p2),p2] 
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

arcs1 = [Filled a]
  where
    a = toArcLine ([Line p1 p1] ++ arcs)
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

eyesWhite = map Filled eyes1

eyes1 = [Circle r1 pt1, Circle r2 pt2]
  where
    pt1 = Point 2.55 1.25
    pt2 = Point 3.5 1.45
    r1 = 0.46
    r2 = 0.37

pupils1 = map Filled [Circle r3 pt3, Circle r4 pt4]
  where
    pt3 = Point 2.7 1.2
    pt4 = Point 3.45 1.35
    r3 = 0.14
    r4 = 0.14

nose1 = map Filled [Circle r1 pt1, Circle r2 pt2]
  where
    pt1 = Point 3.6 (-0.7)
    pt2 = Point 4.0 (-0.6)
    r1 = 0.10
    r2 = 0.10

maxDist c a b =
  if dist c a >= dist c b then a else b

pl1 = PolyLine pts

pts = [pointOfEllipse 3 2 t | t <- [0,dt..twopi]]
  where
    dt = twopi / n
    n = 120


rect1 = Filled (fromRect (Rect (Point (-5) (-4)) (Point 5 4)))

layers1 = [
  Phantom (Point (-8) (-1)) (Point 8 5),
  --Layer "1" Black arcs1 "[line width=0.8pt]",
  --Layer "0" Green [rect1] "",
  --Layer "1" White arcs1 "[line width=0.8pt]",
  --Layer "1" Gray30 head1 "[line width=3.2pt]",
  Layer "2" White legs2 "[line width=10.0pt]",
  Layer "3" White arcs1 "[line width=0.8pt]",
  Layer "1" Gray30 sheepEars2 "[line width=0.8pt]",
  Layer "4" Gray30 sheepHead2 "[line width=0.8pt]",
  Layer "1" White eyesWhite "[line width=0.8pt]",
  Layer "1" Black eyes1 "[line width=1.6pt]",
  Layer "1" Black nose1 "[line width=0.1pt]",
  Layer "1" Black pupils1 "[line width=0.1pt]"
  ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict


