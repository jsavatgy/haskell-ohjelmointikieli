import Eemian

-- | Point3D x y z, RH cartesian coordinates
data Point3D = Point3D Double Double Double

-- |  Spheric3D theta phi, where phi = 
-- polar angle measured from a fixed zenith direction,
-- GeographicNE delta lambda =
-- geographic coordinates, delta=North, lambda=East
data SphericP = Spheric3D Angle Angle 
  | GeographicNE Angle Angle

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

matrix0 alpha = [
  [1,0.5 * cos1 alpha,0],
  [0,0.5 * sin1 alpha,1],
  [0,0,0]
  ]

matrixTimes3 a b = 
  [sum [x * y | (x,y) <- zip a1 b] | a1 <- a]

matr1 pv pAlpha (Point3D x1 y1 z1) = Point x y
  where
    [x,y,z] = matrixTimes3 (matrix1 pv alpha) [x1,y1,z1]
    alpha = DEG pAlpha

r = 1737.1 

orthoYZ (Point3D x y z) = Point y z

perspective = matr1 pv pAlpha
  where
    pv = 8
    pAlpha = 35

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

meridians = [PolyLine [(perspective . cartesian)
  (GeographicNE (DEG d) (DEG l))
  | d <- delta]
    | l <- lambda]
  where
    delta = [-90,-80..90]
    lambda = [-90,-75..90]

latitudes = [PolyLine [(perspective . cartesian)
  (GeographicNE (DEG d) (DEG l))
  | l <- lambda]
    | d <- delta]
  where
    delta = [-90,-75..90]
    lambda = [-90,-70..90]

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
    p1 = Point (-1.05 * r) (-1.05 * r)


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
    p1 = Point (-1.05 * r) (-1.05 * r)

axes = axes1
{-
arrow a b = [Line a c] 
  where
    c = towards1 a b ratio
    ratio = 1.00

axes = 
  arrow ax1 ax2 ++
  [TextE ax2 "$x$" "below left"] ++
  arrow ay1 ay2 ++
  [TextE ay2 "$y$" "above right"] ++
  arrow az1 az2 ++
  [TextE az2 "$z$" "below right"] 
-}
layers1 = [
  Phantom (Point (-s) (-s)) (Point s s),
  --Layer "1" Red [Line p1 p2] "[line width=0.8pt]",
  Layer "1" Black axes "[line width=0.8pt]",
  Layer "1" Black latitudes "[line width=0.8pt]",
  Layer "2" Black meridians "[line width=0.8pt]"]

tpict = tikzPicture layers1

main = do
  putStrLn tpict




