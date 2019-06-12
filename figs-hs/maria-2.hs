import Data.List.Split
import Data.List
import Data.Maybe
import Data.Char
import Eemian

-- | Point3D x y z, RH cartesian coordinates
data Point3D = Point3D Double Double Double

-- |  Spheric3D theta phi, where phi = 
-- polar angle measured from a fixed zenith direction
data Spheric3D = Spheric3D Angle Angle
data GeographicNE = GeographicNE Angle Angle

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

cartesian (Spheric3D lambda delta) = Point3D x y z
  where
    x = r * cos1 theta * sin1 phi
    y = r * sin1 theta * sin1 phi
    z = r * cos1 phi
    theta = lambda
    phi = (DEG 90) `subAngles` delta

meridians = [PolyLine [(perspective . cartesian)
  (Spheric3D (DEG l) (DEG d))
  | d <- delta]
    | l <- visible2 lambda]
  where
    delta = [-90,-75..90]
    lambda = [-180,-165..165]

latitudes = [PolyLine [(perspective . cartesian)
  (Spheric3D (DEG l) (DEG d))
  | l <- visible2 lambda]
    | d <- delta]
  where
    delta = [-90,-75..90]
    lambda = [-180,-165..180]

visible2 = filter (\l -> l >= -74 && l <= 106)

marePg d pos = pg 
  where
    pg = Polygon [
      (perspective .  rotYZ delta lambda . cartesian) 
      (Spheric3D (DEG l) phi)
      | l <- lambdaRim]
    GeographicNE delta lambda = pos
    phi = DEG 90 `subAngles` (RAD theta)
    theta = (d/2) / r
    lambdaRim = [-180,-160..160]

mare2 t = [ FilledWith rgb (marePg d pos) ]
  where
    rgb = RGB v v v
    v = 1.0 - (0.05 + 0.8 * ((g - 3100) / 1000))
    (name,n,e,d,g) = t
    pos = GeographicNE (DEG n) (DEG e)

maria ts = concatMap mare2 (filter visible ts)
  where
    visible (name,n,e,d,g) = e >= -90 && e <= 90 &&
      name == "Oceanus Procellarum"

proper a b c d (Just e) = 
  if f && g then Just e else Nothing
  where 
    f = dist a b + epsilon >= dist a e + dist e b
    g = dist c d + epsilon >= dist c e + dist e d
    epsilon = 0.0001

properIntersection a b c d = let 
    e = intersection a b c d
  in case e of
    Just (Point x y) -> proper a b c d e
    Nothing -> Nothing

pgplIntersections pg pl = [Node x | Just x <- justScs]
  where 
    justScs = [ properIntersection a b c d
      | (a,b) <- zip pg1 (tail pg1 ++ [head pg1]), 
      (c,d) <- zip pl (tail pl) ]
    Polygon pg1 = pg

pgIcs ts vs = concat [
  pgplIntersections (marePg d (
    GeographicNE (DEG n) (DEG e))) v 
  | (name,n,e,d,g) <- ts, PolyLine v <- vs]

intersections1 ts = ics1 ++ ics2
  where
    ics1 = pgIcs ts1 meridians
    ics2 = pgIcs ts1 latitudes
    ts1 = (filter visible ts)
    visible (name,n,e,d,g) = e >= -90 && e <= 90  &&
      name == "Oceanus Procellarum"


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
      (Spheric3D (DEG e) (DEG n))
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
  --Layer "1" Red [Line p1 p2] "[line width=0.8pt]",
  --Layer "1" Black axes "[line width=0.8pt]",
  Layer "2" Gray30 (maria c) "[line width=0.8pt]",
  Layer "2" Black (intersections1 c) "[line width=0.8pt]",
  Layer "3" Black latitudes "[line width=0.8pt]",
  Layer "4" Black meridians "[line width=0.8pt]",
  --Layer "2" Black (names c) "[line width=0.6pt]"
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
  putStrLn (tpict c5)




