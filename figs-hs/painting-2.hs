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
  (-3.01,-2.15), (-5.58,-2.93), (-8.17,-1.88), 
  (-3.52,1.42),  (-4.33,4.44),  (-0.62,5.12),
  (2.63,0.47),   (5.16,-3.41),  (7.90,-1.41) ]

{-"m 94.95,138.60 c -3.01,-2.15 -5.58,-2.93 -8.17,-1.88 
  -3.52,1.42 -4.33,4.44 -0.62,5.12 2.63,0.47 5.16,-3.41 
  7.90,-1.41" -}

earL = [
  (4.02,-3.10), (5.25,-2.31),  (7.95,-2.05),
  (3.36,0.31),  (4.34,5.09),   (0.99,5.20),
  (-1.57,0.05), (-5.52,-2.36), (-6.92,-1.44) ]

{-"m 102.91,136.48 c 4.02,-3.10 5.25,-2.31 7.95,-2.05 
   3.36,0.31 4.34,5.09 0.99,5.20 -1.57,0.05 -5.52,-2.36 
   -6.92,-1.44" -}

sheepEars2 = map (Filled . Polygon) [right,left]
  where
    right = map (addCoords ptR . scaleCoords 0.2) right0
    left  = map (addCoords ptL . scaleCoords 0.2) left0
    ptR = Point 1.94 1.85
    ptL = Point 3.61 2.27
    [right0,left0] = map sheepEar [earR,earL]

sheepHead1 = [mkPoint (bezier p0 p1 p2 p3 t) 
  | (p0,p1,p2,p3) <- headPts1, t <- [0.0,0.05..0.95]]

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
legs2 = map (Filled . Polygon) legPts1

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

widen pts = [concat [wup pt1,wdn pt2] | [pt1,pt2] <- pts]
  where
    wup pt = [pt `addCoords` ext, pt `subCoords` ext]
    wdn pt = [pt `subCoords` ext, pt `addCoords` ext]
    ext = Point 0.22 0

legPts1 = widen 
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
    g = mkStdGen 4798395385
    n = 15

-- dots2 = [Filled (Circle 0.15 p) | p <- outer ++ [center]]
dots2 = outer
  where
    outer = [maxDist center p1 p2 | [p1,p2] <- xs]
    xs = [circleCircleIntersections c d 
      | (c,d) <- zip c1 (tail c1)]
    c1 = circles1 ++ [head circles1]

center = Point 0 0

eyesWhite  = map Filled (eyes1 1)
eyesBorder = map Filled (eyes1 2)

eyes1 i = [Circle r1 pt1, Circle r2 pt2]
  where
    pt1 = Point 2.55 1.25
    pt2 = Point 3.5 1.45
    (r1,r2) = if i==1 then (0.42,0.33) else (0.50,0.41)

pupils1 = map Filled [Circle r3 pt3, Circle r4 pt4]
  where
    pt3 = Point 2.7 1.2
    pt4 = Point 3.45 1.35
    (r3,r4) = (0.14,0.14)

nose1 = map Filled [Circle r1 pt1, Circle r2 pt2]
  where
    pt1 = Point 3.6 (-0.7)
    pt2 = Point 4.0 (-0.6)
    (r1,r2) = (0.10,0.10)

maxDist c a b =
  if dist c a >= dist c b then a else b

pl1 = PolyLine pts

pts = [pointOfEllipse 3 2 t | t <- [0,dt..twopi]]
  where
    dt = twopi / n
    n = 120

sandyBezier yz nx rx = [mkPoint (bezier p0 p1 p2 p3 t) 
  | [p0,p1,p2,p3] <- sandyPts yz nx rx, t <- [0.0,0.1..1.0]]

sandyPts yz nx rx = gr3
  where
    gr3 = [[last g1] ++ tail g2 
      | (g1,g2) <- zip gr2 (tail gr2)]
    gr2 = [g1 ++ [midPt [last g1,head (tail g2)]]
      | (g1,g2) <- zip gr (tail gr)]
    gr = chunksOf 3 pts2
    --pts2 = replicate 3 (head pts1) ++ pts1 ++ 
    pts2 = replicate 3 dummyX ++ pts1 ++ 
      replicate 3 (last pts1) 
    pts1 = [(x,y-(yz-6)) | (x,y) <- zip xs rands]
    dummyX = (-9 - dx,rands !! 90 -(yz-6))
    xs = take (round ns) [-9,dx-9..]
    dx = (xMax - xMin) / (ns - 1)
    (xMin,xMax,ns) = (-9,9,nx*3)
    rands = randomRs (-rx,rx) g
    g = mkStdGen (round (9484845 + yz * nx / rx))

sonne1 = Filled $ Circle 2.8 (Point 3 9.5)

midPt pts = toTuple t2
  where
    t2 = midpoint t1
    t1 = map mkPoint pts

(botPt1,botPt2) =
  (Point pageMinX pageMinY,Point pageMaxX pageMinY)
(pagePt1,pagePt2) = 
  (Point pageMinX pageMinY,Point pageMaxX pageMaxY)
(pageMinX,pageMaxX) = (-9,9)
(pageMinY,pageMaxY) = (-13,12.7)

ground1 y nx rx = [Filled $ Polygon ([botPt1] ++ 
  sandyBezier y nx rx ++ [botPt2])]

[sandy1,vege1,vege2,vege3,vege4] = [
  (ground1 10 15 0.40),
  (ground1 6 6 0.30),
  (ground1 3 9 0.40),
  (ground1 1 12 0.50),
  (ground1 0 18 0.60) ]
--sandy2 = [Circle 0.05 (Point x y) | (x,y) <- concat sandyPts]

--rect1 = Filled (fromRect (Rect (Point (-9) (-13)) (Point 9 12.7)))
rect1 = Filled (fromRect (Rect pagePt1 pagePt2))


sheepM (Layer name color s attr) 
  | name == "S" = sheepM1 (Layer name color s attr)
  | otherwise   = Layer name color s attr

sheepM1 (Layer name color s attr) =
  Layer name color (map sheeped s) attr
  where
    sheeped2 (ArcElem r start end) =
      ArcElem (dx r) start end
    sheeped2 (LineElem p1 p2) =
      LineElem (px p1) (px p2)
    sheeped (ArcLine xs) =
      ArcLine [sheeped2 x | x <- xs]
    sheeped (Filled x) = 
      Filled (sheeped x)
    sheeped (FilledWith (RGB r g b) x) = 
      FilledWith (RGB r g b) (sheeped x)
    sheeped (Circle r pt) = 
      Circle (dx r) (px pt)
    sheeped (Arc r p start end) =
      Arc (dx r) (px p) start end
    sheeped (Line p1 p2) =
      Line (px p1) (px p2)
    sheeped (Polygon ps) =
      Polygon [px p | p <- ps]
    sheeped (PolyLine ps) =
      PolyLine [px p | p <- ps]
    sheeped x = x
    px pt = pt `subCoords` p1
    dx r = r
    p1 = Point 1.5 4

layers1 = [
  --Phantom (Point (-8) (-1)) (Point 14 5),
  Layer "2" Air1 [rect1] "[line width=0.1pt]",
  Layer "3" Sonne1 [sonne1] "[line width=0.1pt]",
  Layer "1" Green4 vege4 "[line width=0.1pt]",
  Layer "1" Green3 vege3 "[line width=0.1pt]",
  Layer "1" Green2 vege2 "[line width=0.1pt]",
  Layer "1" Green1 vege1 "[line width=0.1pt]",
  Layer "1" Sand1 sandy1 "[line width=0.1pt]",
  Layer "S" White legs2 "[line width=1.1pt]",
  Layer "S" White arcs1 "[line width=0.1pt]",
  Layer "S" Gray40 sheepEars2 "[line width=0.1pt]",
  Layer "S" Gray40 sheepHead2 "[line width=0.1pt]",
  Layer "S" Black eyesBorder "[line width=0.1pt]",
  Layer "S" White eyesWhite "[line width=0.1pt]",
  Layer "S" Black nose1 "[line width=0.1pt]",
  Layer "S" Black pupils1 "[line width=0.1pt]"

  --Layer "1" Gray50 sandy2 "[line width=5.0pt]",
  ]

--A5= 148 × 210 mm 
tpict = tikzPictureWithScale (map sheepM layers1) 0.82

main = do
  putStrLn tpict


