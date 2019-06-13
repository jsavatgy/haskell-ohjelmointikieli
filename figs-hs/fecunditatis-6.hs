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

fecunditatis4 = Polygon fc4

fc4 = concat [new io1 io2 p1 p2 
  | ((io1,io2),(p1,p2)) <- zip io ia]
  where
    (s1,s2) = zip g1 (tail g1) !! 2
    g1 = gridGreatCircles !! 0
    io = around (insideOutside fecunditatis3 !! 2)
    ia = around pg
    Polygon pg = fecunditatis3
    new In In p1 p2 = [p1]
    new In Out p1 p2 = [p1,
      fromJust (intersection s1 s2 p1 p2)]
    new Out Out p1 p2 = []
    new Out In p1 p2 = [
      fromJust (intersection s1 s2 p1 p2)]



fecunditatis3 = Polygon fc3

fc3 = concat [new io1 io2 p1 p2 
  | ((io1,io2),(p1,p2)) <- zip io ia]
  where
    (s1,s2) = zip square1 (tail square1) !! 1
    square1 = gridGreatCircles !! 0
    io = around (insideOutside fecunditatis !! 1)
    ia = around pg
    Polygon pg = fecunditatis
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

insideOutside pg = [[(inOut . sign . area . Polygon) [a,b,c]
   | c <- f] | (a,b) <- zip g1 (tail g1)]
  where
    g1 = head gridGreatCircles
    Polygon f = pg
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

grid2 = PolyLine [pg !! 2, pg !! 3]
  where
    pg = head gridGreatCircles

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

p1 = Point (-r) (-r)
p2 = Point r r
s = 4 * r

mare d pos = Polygon [ pt0 `addCoords`
  pointFromPolar (DEG l) r2 | l <- lambdaRim]
  where
    r2 = (d/2) / (twopi * r / 360)
    pt0 = equirectangular (Spheric3D lambda delta)
    GeographicNE delta lambda = pos
    lambdaRim = [-180,-140..140]

sx = 90

ptLegends = 
  [Text pt ("$p_{" ++ show n ++ "}$") | (n,pt) <- zip [1..] pg3] ++
  [Text pt ("$\\bullet$") | (n,pt) <- zip [1..] pg4]
  where
    pg3 = takeIndex pg2
    pg4 = takeIndex pg
    Polygon pg2 = fecunditatis2
    Polygon pg = fecunditatis
    idx = [1,2,9]
    takeIndex xs = [x | (n,x) <- zip [1..] xs, n `elem` idx]


ptLegends2 = [ 
  NamedSymbPos "$s_{1}$" "$\\bullet$" SW n1,
  NamedSymbPos "$s_{2}$" "$\\bullet$" SE n2,
  --NamedSymbPos "$s_{3}$" "$\\bullet$" NE n3,
  NamedSymbPos "$s_{4}$" "$\\bullet$" NW n4
  ]
  where
    [n1,n2,n3,n4] = map Node pg
    Polygon pg = grid1

iPts1 = [ 
  NamedSymbPos "$i_{1}$" "$\\bullet$" SE n1,
  NamedSymbPos "$i_{2}$" "$\\bullet$" NE n2,
  NamedSymbPos "$i_{3}$" "$\\bullet$" NE n3,
  NamedSymbPos "$i_{4}$" "$\\bullet$" NE n4
  ]
  where
    [n1,n2] = map Node [pg1 !! 2, pg1 !! 3]
    [n3,n4] = map Node [pg2 !! 0, pg2 !! 3]
    Polygon pg1 = fecunditatis3
    Polygon pg2 = fecunditatis4

ptLegends3 = [ 
  NamedSymbPos (show n) "$\\bullet$" E p | (n,p) <- zip [1..] ps
  ]
  where
    ps = map Node pg
    Polygon pg = fecunditatis4


fecunditatis2 = mare d pos
  where
    d = 1150 
    pos = GeographicNE (DEG (-7.8)) (DEG 51.3)

layers1 = [
  Phantom (Point (-sx) (-sx)) (Point sx sx),
  --Layer "1" Red [Line p1 p2] "[line width=0.8pt]",
  Layer "2" Gray20 (map Filled [fecunditatis4]) "[line width=0.8pt]",
  Layer "2" Black [fecunditatis3] "[line width=0.8pt]",
  --Layer "3" Black latitudes "[line width=0.8pt]",
  --Layer "4" Black meridians "[line width=0.8pt]",
  Layer "4" Black [grid1] "[line width=0.8pt,dashed]",
  Layer "1" Black ptLegends "[line width=0.8pt]",
  Layer "1" Black ptLegends2 "[line width=0.8pt]",
  Layer "1" Black iPts1 "[line width=0.8pt]",
  --Layer "1" Red ptLegends3 "[line width=0.8pt]",
  Empty
  ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict


