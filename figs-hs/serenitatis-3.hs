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
  (GeographicNE (DEG d) (DEG l))
  | d <- delta]
    | l <- lambda]
  where
    delta = [-90,-75..90]
    lambda = [-90,-75..90]

latitudes = [PolyLine [(perspective . cartesian)
  (GeographicNE (DEG d) (DEG l))
  | d <- delta]
    | l <- lambda]
  where
    delta = [-90,-75..90]
    lambda = [-90,-75..90]

marePg1 d pos = Filled $ Polygon $ marePts d pos

marePts d pos = [ perspective $ rotYZ delta lambda $ 
  cartesian $ Spheric3D (DEG l) phi | l <- lambdaRim ]
  where
    GeographicNE delta lambda = pos
    phi = DEG 90 `subAngles` (RAD theta)
    theta = (d/2) / r
    lambdaRim = [-180,-160..160]

serenitatis = marePg1 d pos
  where
    d = 707 
    pos = GeographicNE (DEG 28) (DEG 17.5)

procellarum = marePg1 d pos
  where
    d = 2568 
    pos = GeographicNE (DEG 18.4) (DEG (-57.4))

proto1 = marePg1 d pos
  where
    d = 100 
    pos = GeographicNE (DEG 0) (DEG (-90))

proto0 = marePg1 160 (GeographicNE (DEG 0) (DEG 0))

proto2 = marePg1 d pos
  where
    d = 100
    pos = GeographicNE (DEG 0) (DEG 90)

p1 = Point (-r) (-r)
p2 = Point r r
s = 4 * r

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
    p1 = Point (-1.10 * r) (-1.10 * r)

arrow a b = Line a c 
  where
    c = towards1 a b ratio
    ratio = 1.00

axisText txt pt1 pt2 = TextE pt2 txt pos
  where
    Point x1 y1 = pt1
    Point x2 y2 = pt2
    pos
     | x1 == x2 = "below right"
     | y1 == y2 = "below left"
     | x1 < x2  = "above right"
     | x2 < x1  = "above left"
     | otherwise = "right"

axes1 = [
  arrow or1 ax2,
  arrow or1 ay2,
  arrow or1 az2,
  axisText "$x$" or1 ax2,
  axisText "$y$" or1 ay2,
  axisText "$z$" or1 az2 ]
  where
    [or1,ax2,ay2,az2] = map (addCoords p1) xs 
    xs = map perspective [o1,x2,y2,z2]
    o1 = Point3D 0 0 0
    x2 = Point3D a 0 0
    y2 = Point3D 0 a 0
    z2 = Point3D 0 0 a
    a = 1000
    p1 = Point (-1.2 * r) (-1.05 * r)

axes = axes1

layers1 = [
  Phantom (Point (-s) (-s)) (Point s s),
  --Layer "1" Red [Line p1 p2] "[line width=0.8pt]",
  Layer "1" Black axes "[line width=0.8pt]",
  Layer "2" Gray20 [procellarum] "[line width=0.8pt]",
  Layer "2" Gray30 [serenitatis] "[line width=0.8pt]",
  Layer "3" Black latitudes "[line width=0.8pt]",
  Layer "4" Black meridians "[line width=0.8pt]",
  Layer "2" Red [proto0] "[line width=0.8pt]"
  ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict




