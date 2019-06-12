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

matrix0 alpha = [
  [1,0,0.5 * cos1 alpha],
  [0,1,0.5 * sin1 alpha],
  [0,0,0]]

matrixI = [
  [1,0,0],
  [0,1,0],
  [0,0,1]]

matr1 (Point3D x1 y1 z1) = Point x y
  where
    [x,y,z] = matrixTimes3 (matrix0 alpha) [x1,y1,z1]
    alpha = DEG 30

matr2 (Point3D x1 y1 z1) = Point y z
  where
    [x,y,z] = matrixTimes3 matrixI [x1,y1,z1]

dropX (Point3D x y z) = Point y z

perspective = matr2





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


matrixTimes3 a b = 
  [sum [x * y | (x,y) <- zip a1 b] | a1 <- a]

r = 1737.1 

equirectangular (Spheric3D lambda delta) = Point l d
  where
    DEG l = degrees lambda
    DEG d = degrees delta

cartesian (Spheric3D lambda delta) = Point3D x y z
  where
    x = r * cos1 theta * sin1 phi
    y = r * sin1 theta * sin1 phi
    z = r * cos1 phi
    theta = lambda
    phi = (DEG 90) `subAngles` delta

data InOut = In | Out
  deriving Show

sign x = if x < 0 then (-1) else 1
around xs = zip xs ((tail . cycle) xs)

insideOutside = [[(inOut . sign . area . Polygon) [a,b,c]
   | c <- f] | (a,b) <- zip g1 (tail g1)]
  where
    g1 = head gridGreatCircles
    Polygon f = fecunditatis
    inOut   1  = In
    inOut (-1) = Out

gridGreatCircles = concat [[[
  xpt l1 d1, xpt l2 d1, xpt l2 d2, xpt l1 d2]
  | (d1,d2) <- zip vb3 (tail vb3)]
    | (l1,l2) <- zip vb2 (tail vb2)]
  where
    xpt l d = equirectangular (Spheric3D (DEG l) (DEG d))
    vb3 = visible3 delta
    vb2 = visible2 lambda
    delta = [-90,-75..90]
    lambda = [-90,-75..90]

grid1 = Polygon pg
  where
    pg = head gridGreatCircles

meridians = [PolyLine [equirectangular
  (Spheric3D (DEG l) (DEG d))
  | d <- visible3 delta]
    | l <- visible2 lambda]
  where
    delta = [-90,-75..90]
    lambda = [-90,-75..90]

latitudes = [PolyLine [equirectangular
  (Spheric3D (DEG l) (DEG d))
  | l <- visible2 lambda]
    | d <- visible3 delta]
  where
    delta = [-90,-75..90]
    lambda = [-90,-75..90]

visible2 = filter (\l -> l >= 29 && l <= 76)
visible3 = filter (\d -> d >= -31 && d <= 16)

fecunditatis = mare d pos
  where
    d = 909 
    pos = GeographicNE (DEG (-7.8)) (DEG 51.3)

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


mare d pos = Polygon [ pt0 `addCoords`
  pointFromPolar (DEG l) r2 | l <- lambdaRim]
  where
    r2 = (d/2) / (twopi * r / 360)
    pt0 = equirectangular (Spheric3D lambda delta)
    GeographicNE delta lambda = pos
    lambdaRim = [-180,-140..140]

backto xs = (intercalate "\t" xs1)
  where
    xs1 = xs

tabulated str = map trim (splitOn "\t" str)


[ax1,ax2,ay1,ay2,az1,az2] = map (addCoords p1) xs 
  where
    xs = map perspective [x1,x2,y1,y2,z1,z2]
    x1 = Point3D b 0 0
    x2 = Point3D a 0 0
    y1 = Point3D 0 b 0
    y2 = Point3D 0 a 0
    z1 = Point3D 0 0 b
    z2 = Point3D 0 0 a
    a = 1000
    b = 0
    p1 = Point (-1.4 * r) (-1.00 * r)

arrow a b = [Line a c] 
  where
    c = towards1 a b ratio
    ratio = 1.00

axes = 
  -- arrow ax1 ax2 ++
  -- [TextE ax2 "$\\cancel{x}$" "above right"] ++
  arrow ay1 ay2 ++
  [TextE ay2 "$y$" "above left"] ++
  arrow az1 az2 ++
  [TextE az2 "$z$" "below right"] 

sx = 90

ptLegends2 = [ 
  NamedSymbPos "$s_{1}$" "$\\bullet$" SW n1,
  NamedSymbPos "$s_{2}$" "$\\bullet$" SE n2,
  NamedSymbPos "$s_{3}$" "$\\bullet$" NE n3,
  NamedSymbPos "$s_{4}$" "$\\bullet$" NW n4
  ]
  where
    [n1,n2,n3,n4] = map Node pg
    Polygon pg = grid1

ptLegends = 
  [Text pt ("$p_{" ++ show n ++ "}$") | (n,pt) <- zip [1..] f] ++
  [Text pt ("$\\bullet$") | (n,pt) <- zip [1..] g]
  where
    Polygon f = fecunditatis2
    Polygon g = fecunditatis

fecunditatis2 = mare d pos
  where
    d = 1150 
    pos = GeographicNE (DEG (-7.8)) (DEG 51.3)

layers1 = [
  Phantom (Point (-sx) (-sx)) (Point sx sx),
  --Layer "1" Red [Line p1 p2] "[line width=0.8pt]",
  --Layer "1" Black axes "[line width=0.8pt]",
  Layer "2" Gray20 (map Filled [fecunditatis]) "[line width=0.8pt]",
  Layer "2" Black [fecunditatis] "[line width=0.8pt]",
  --Layer "3" Black latitudes "[line width=0.8pt]",
  --Layer "4" Black meridians "[line width=0.8pt]",
  Layer "4" Black [grid1] "[line width=0.8pt,dashed]",
  Layer "1" Black ptLegends "[line width=0.8pt]",
  Layer "1" Black ptLegends2 "[line width=0.8pt]",
  Empty
  ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict


