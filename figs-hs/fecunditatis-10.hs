import Data.List
import Data.Maybe
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

equirect (GeographicNE delta lambda)  = Point l d
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
    lambda = [-90,-75..90]

fc sq 0 = fecunditatis
fc sq n = nextGen1 (fc sq nm) ((around sq) !! nm)
  where
    nm = n - 1
    nextGen1 f (a,b) = nextGen f a b

blocks = b2
  where
    b2 = filter (not . null) b1
    b1 = map block1 squares
    block1 sq = fc sq 4
    squares =  gridGreatCircles 

meridians = [PolyLine [equirect
  (GeographicNE (DEG d) (DEG l))
  | d <- visible3 delta]
    | l <- visible2 lambda]
  where
    delta = [-90,-75..90]
    lambda = [-90,-75..90]

latitudes = [PolyLine [equirect
  (GeographicNE (DEG d) (DEG l))
  | l <- visible2 lambda]
    | d <- visible3 delta]
  where
    delta = [-90,-75..90]
    lambda = [-90,-75..90]

visible2 = filter (\l -> l > 29 && l < 76)
visible3 = filter (\d -> d > -31 && d < 16)

fecunditatis = marePts d pos
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

marePts d pos = [ pt0 `addCoords`
  pointFromPolar (DEG l) r2 | l <- lambdaRim ]
  where
    r2 = (d/2) / (twopi * r / 360)
    pt0 = equirect (GeographicNE delta lambda)
    GeographicNE delta lambda = pos
    lambdaRim = [-180,-140..140]

sx = 90

blocksA = c
  where
    c = [map (`addCoords` Point x y) bl
      | (bl,(x,y)) <- b]
    b = zip blocks [(x,y) | x <- [-1..1], y <-[-1..1]]

layers1 = [
  Phantom (Point (-sx) (-sx)) (Point sx sx),
  Layer "2" Gray20 (map (Filled . Polygon) blocksA) "[line width=0.8pt]",
  Layer "2" Black (map Polygon blocksA) "[line width=0.8pt,line join=round,line cap=round]",
  --Layer "3" Black latitudes "[line width=0.8pt]",
  --Layer "4" Black meridians "[line width=0.8pt]",
  Empty
  ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict


