import Data.List.Split
import Data.List
import Data.Maybe
import Data.Char
import Eemian

-- | Point3D x y z, RH cartesian coordinates
data Point3D = Point3D Double Double Double

-- |  Spheric3D theta phi, where phi = 
-- polar angle measured from a fixed zenith direction,
-- GeographicNE delta lambda =
-- geographic coordinates, delta=North, lambda=East
data SphericP = Spheric3D Angle Angle 
  | GeographicNE Angle Angle

rotationX t = [
  [1,      0,       0],
  [0, cos1 t, -sin1 t],
  [0, sin1 t,  cos1 t]
  ] 

rotationY t = [
  [cos1 t,  0, sin1 t],
  [0,       1,      0],
  [-sin1 t, 0, cos1 t]
  ] 

rotationZ t = [
  [cos1 t, -sin1 t, 0],
  [sin1 t,  cos1 t, 0],
  [     0,       0, 1]
  ]

rotYZ delta lambda (Point3D x1 y1 z1) = Point3D x y z
  where
    [x,y,z] = foldr matrixTimes3 [x1,y1,z1] rts
    rts = [rotationZ lambda,rotationY phi]
    phi = DEG 90 `subAngles` delta

matrix1 m alpha = case m of
  1 -> [ [1,0,a], [0,1,b], z]
  2 -> [ [1,a,0], [0,b,1], z]
  3 -> [ [a,1,0], [b,0,1], z]
  4 -> [ [0,1,a], [1,0,b], z]
  5 -> [ [0,a,1], [1,b,0], z]
  6 -> [ [a,0,1], [b,1,0], z]
  7 -> [ [1,c,0], [0,d,1], z]
  8 -> [ [1,0,c], [0,1,d], z]
  9 -> [ [0,1,c], [1,0,d], z]
  10 -> [ [c,1,0], [d,0,1], z]
  11 -> [ [c,0,1], [d,1,0], z]
  12 -> [ [0,c,1], [1,d,0], z]
  where
    a = 0.5 * cos1 alpha
    b = 0.5 * sin1 alpha
    c = -a; d = -b
    z = [0,0,0]

matr1 pv pAlpha (Point3D x1 y1 z1) = Point x y
  where
    [x,y,z] = matrixTimes3 (matrix1 pv alpha) [x1,y1,z1]
    alpha = DEG pAlpha

matrixTimes3 a b = 
  [sum [x * y | (x,y) <- zip a1 b] | a1 <- a]

r = 1737.1 

dropX (Point3D x y z) = Point y z

perspective = matr1 pv pAlpha
  where
    pv = 10
    pAlpha = 35

meridians = [PolyLine [(perspective . cartesian)
  (GeographicNE (DEG d) (DEG l))
  | d <- delta]
    | l <- visible2 lambda]
  where
    delta = [-90,-75..90]
    lambda = [-180,-165..165]

latitudes = [PolyLine [(perspective . cartesian)
  (GeographicNE (DEG d) (DEG l))
  | l <- visible2 lambda]
    | d <- delta]
  where
    delta = [-90,-75..90]
    lambda = [-180,-165..180]

visible2 = filter (\l -> l > -74 && l < 106)
visible3 = filter (\d -> d > -91 && d < 91)

equirect (GeographicNE delta lambda) = Point l d
  where
    DEG d = degrees delta
    DEG l = degrees lambda

ptToSpheric3D (Point x y) = Spheric3D theta phi
  where
    theta = lambda
    phi = (DEG 90) `subAngles` delta
    delta = DEG y
    lambda = DEG x

cartesian (GeographicNE delta lambda) = 
  cartesian (Spheric3D theta phi)  
  where
    theta = lambda
    phi = (DEG 90) `subAngles` delta

cartesian (Spheric3D theta phi) = Point3D x y z
  where
    x = r * cos1 theta * sin1 phi
    y = r * sin1 theta * sin1 phi
    z = r * cos1 phi

nextGen fc s1 s2 = concat [new i1 i2 p1 p2 
  | ((i1,i2),(p1,p2)) <- zip io2 pts]
  where
    io1 = inOut1 s1 s2 fc
    io2 = around io1
    pts = around fc
    new In In p1 p2 = [p1]
    new In Out p1 p2 = [p1,
      fromJust (intersection s1 s2 p1 p2)]
    new Out Out p1 p2 = []
    new Out In p1 p2 = [
      fromJust (intersection s1 s2 p1 p2)]

data InOut = In | Out
  deriving Show

sign x = if x < 0 then (-1) else 1
around xs = zip xs ((tail . cycle) xs)

inOut1 p1 p2 pts = [
  (inOut . sign . area . Polygon) [p1,p2,p3] | p3 <- pts]
  where
    inOut   1  = In
    inOut (-1) = Out

gridGreatCircles = concat [[[
  xpt l1 d1, xpt l2 d1, xpt l2 d2, xpt l1 d2]
  | (d1,d2) <- zip vb3 (tail vb3)]
    | (l1,l2) <- zip vb2 (tail vb2)]
  where
    xpt l d = equirect (GeographicNE (DEG d) (DEG l))
    vb3 = visible3 delta
    vb2 = visible2 lambda
    delta = [-90,-75..90]
    lambda = [-180,-165..180]

{-

Fhe conversion can be considered as two sequential rectangular to polar conversions: 

(1) the first in the Cartesian xy-plane from (x,y) to (R,φ), where R is the projection of r onto the xy-plane, and 

(2) the second in the Cartesian zR-plane from (z,R) to (r,θ). 

The correct quadrants for φ and θ are implied by the correctness of the planar rectangular to polar conversions

-}

fc mre sq 0 = mre
fc mre sq n = nextGen1 (fc mre sq nm) ((around sq) !! nm)
  where
    nm = n - 1
    nextGen1 f (a,b) = nextGen f a b

blocksEqui mre = b2
  where
    b2 = filter (not . null) b1
    b1 = map (blockE mre) squares
    blockE mre sq = fc mre sq 4
    squares =  gridGreatCircles 

cutEqui mre = blocksEqui mre

geog (Point3D x y z) = GeographicNE delta lambda
  where
    delta = directionAngle vect2
    vect2 = Vector rProjXy z
    rProjXy = sqrt (sqr x + sqr y)
    lambda = theta
    theta = directionAngle vect1 
    vect1 = Vector x y
    r = sqrt (sqr x + sqr y + sqr z)
    sqr x = x * x

marePts d pos = [ equirect pos `addCoords`
  pointFromPolar (DEG l) r2 | l <- lambdaRim ]
  where
    r2 = (d/2) / (twopi * r / 360)
    lambdaRim = [-180,-160..160]

marePg d pos = pg 
  where
    pg = map Polygon blocks2
    blocks2 = [map (perspective . cartesian . 
      ptToSpheric3D) pts | pts <- blocks1]
    blocks1 = cutEqui pts3
    pts3 = map (equirect . geog) pts2
    pts2 = [ (rotYZ delta lambda . cartesian) 
      (Spheric3D (DEG th) phi)
      | th <- lambdaRim]
    GeographicNE delta lambda = pos
    phi = RAD ((d/2) / r)
    lambdaRim = [-180,-160..160]

mare2 t = map (FilledWith rgb) (marePg d pos)
  where
    rgb = RGB v v v
    v = 1.0 - (0.05 + 0.8 * ((g - 3100) / 1000))
    (name,n,e,d,g) = t
    pos = GeographicNE (DEG n) (DEG e)

maria ts = ts2
  where
    ts2 = concatMap mare2 (filter visible ts)
    visible (name,n,e,d,g) = e >= -90 && e <= 90 
    -- && name 
    -- `elem` ["Oceanus Procellarum","Mare Fecunditatis"]

-- | http://mathworld.wolfram.com/PolygonArea.html
-- Polygon area: vertices counterclockwise
pseudoArea pts = half (sum [det [[x1,y1],[x2,y2]] 
  | (Point x1 y1,Point x2 y2) 
    <- zip pts (tail pts ++ take 1 pts)])
 
half = (0.5 *)
det [[a,b],[c,d]] = a * d - b * c

data LeftRight = L | R
  deriving Eq

getSide (Point x1 y1) (Point x2 y2)
  | x1 <= x2  = L
  | otherwise = R

name2 t = (name,side,posXY)
  where
    side = getSide posXY pt1
    pt1 = Point (-70) (-70)
    posXY = (perspective . cartesian) 
      (GeographicNE (DEG n) (DEG e))
    (name,n,e,d,g) = t

sortOnY = sortOn (\(n,s,Point x y) -> -y) 

refineOrder L xs = [xs !! n | n <- [0,2,1,3,4,5,6,7,8]]
refineOrder R xs = [xs !! n | n <- [0,1,3,2,4,5,7,6,8,9]]

names ts = concat (lx ++ rx)
  where
    lx = map (\(n,s,p,pt) -> [Texttt pt n "left",
      Arrow "" pt p]) l3
    rx = map (\(n,s,p,pt) -> [Texttt pt n "right",
      Arrow "" pt p]) r3
    l3 = map f3 l2
    r3 = map f3 r2
    f3 = \(y,(n,s,p)) -> (n,s,p,crc s y)
    l2 = zip [dy*sL-d,dy*(sL-1)-d..] l1
    r2 = zip [dy*sR-d,dy*(sR-1)-d..] r1
    d = 70
    sL = intToDouble (length l1) / 2
    sR = intToDouble (length r1) / 2
    dy = 2 * r / max ln rn
    [ln,rn] = map (intToDouble . length) [left,right]
    l1= refineOrder L l0
    r1= refineOrder R r0
    [l0,r0] = map sortOnY [left,right]
    right = filter (\(n,s,p) -> s == R) n1
    left  = filter (\(n,s,p) -> s == L) n1
    n1 = map name2 (filter visible ts)
    visible (name,n,e,d,g) = e > -90 && e < 90 

crc s y = p4
  where
    p4 = if d1 < d2 then p1 else p2
    [d1,d2] = map (dist (p3 s)) [p1,p2]
    [p1,p2] = intersect1 circle1 pt1 pt2
    pt1 = Point (-r) y
    p0 = Point (-70) (-70)
    circle1 = Circle (r+300) p0
    pt2 = Point r y
    p3 L = pt1
    p3 R = pt2

intersect1 circle1 pt1 pt2 = p h
  where
    h = circleLineIntersections circle1 pt1 pt2
    p [] = [pt1,pt2]
    p [a] = [a,pt2]
    p [a,b] = [a,b]

p1 = Point (-r) (-r)
p2 = Point r r
s = 4 * r

trim = dropWhileEnd (not . isAlphaNum) .
  dropWhile (not . isAlphaNum)

aged str = (a,b)
  where
    [a,b] = tb
    tb = splitOn "\t" str 

lookup1 table2 table1 = [a,c1,d1,e,f]
  where
    [c1,d1] = map brt [c,d]
    [a,b,c,d,e] = table1
    f = case lookup a table2 of
      Just x -> x
      Nothing -> ""
    brt s = addMinus s ++ takeWhile (/= ' ') s
    addMinus s = if last s `elem` "SW" then "-" else ""

legendByr = [Texttt (Point (-1700) (-1800)) 
  "Surface basalt (Byr)" "left"]

legendA = concat [[(FilledWith (rgb a)) r,r,
  Texttt pt (show1 (a/1000)) "right"]
  | (r,a,pt) <- zip3 rs ages ps]
  where
    rs = [ fromRect ( Rect (pt1 x) (pt2 x)) | x <- xs ]
    ps = [ pt3 x | x <- xs ]

    pt3 x = Point (1500*x-4500) (-2150)
    pt1 x = Point (1500*x-5000) (-2000)
    pt2 x = Point (1500*x-4500) (-2300)
    n = 6
    xs = [1..n]
    rgb a = RGB (v a) (v a) (v a)
    v g = 1.0 - (0.05 + 0.8 * ((g - 3100) / 1000))
    ages = [3100,3300..4100]

backto xs = (intercalate "\t" xs1)
  where
    xs1 = xs

tabulated str = map trim (splitOn "\t" str)

aged2 xs = (a,b1,c1,d1,e1)
  where
    [a,b,c,d,e] = xs
    [b1,c1,d1,e1] = map readd [b,c,d,e] 
    readd x = read x :: Double

sortOnFifth = sortOn (\(a,b,c,d,e) -> e)
valid1 [a,b,c,d,e] = e /= ""

layers1 c = [
  Phantom (Point (-s) (-s)) (Point s s),
  Layer "2" Gray30 (maria c) "[line width=0.8pt]",
  Layer "3" Black latitudes "[line width=0.8pt]",
  Layer "4" Black meridians "[line width=0.8pt]",
  Layer "2" Black (names c) "[line width=0.6pt]",
  Layer "2" Black legendA "[line width=0.8pt]",
  Layer "2" Black legendByr "[line width=0.8pt]",
  Empty
  ]

tpict c = tikzPicture (layers1 c)

main = do
  content <- readFile "../moon-random/moon-list.txt"
  content2 <- readFile "../moon-random/surface-basalt-age.txt"
  let 
    moon = filter (not . null) (lines content)
    ageText = filter (not . null) (lines content2)
    c1 = map tabulated moon
    aged1 = map aged ageText
    c2 = map (lookup1 aged1) c1
    c3 = filter valid1 c2
    c4 = map aged2 c3
    c5 = sortOnFifth c4
  putStrLn (tpict c4)


