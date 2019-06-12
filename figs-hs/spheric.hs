import Eemian

-- | Point3D x y z, RH cartesian coordinates
data Point3D = Point3D Double Double Double

-- |  Spheric3D theta phi, where phi = 
-- polar angle measured from a fixed zenith direction
data Spheric3D = Spheric3D Angle Angle

matrix1 alpha = [
  [1,0,0.5 * cos1 alpha],
  [0,1,0.5 * sin1 alpha],
  [0,0,0]]

matrix2 = [
  [1,0,0],
  [0,1,0],
  [0,0,1]]


matrixTimes3 a b = 
  [sum [x * y | (x,y) <- zip a1 b] | a1 <- a]

matr1 (Point3D x1 y1 z1) = Point x y
  where
    [x,y,z] = matrixTimes3 (matrix1 alpha) [x1,y1,z1]
    alpha = DEG 30

matr2 (Point3D x1 y1 z1) = Point y z
  where
    [x,y,z] = matrixTimes3 matrix2 [x1,y1,z1]

r = 1737.1 

dropX (Point3D x y z) = Point y z

perspective = matr2

cartesian (Spheric3D lambda delta) = Point3D x y z
  where
    x = r * cos1 theta * sin1 phi
    y = r * sin1 theta * sin1 phi
    z = r * cos1 phi
    theta = lambda
    phi = (DEG 90) `subAngles` delta

-- $$\begin{aligned}x&=r\sin \theta \cos \phi \\y&=r\sin \theta \sin \phi \\z&=r\cos \theta \end{aligned}$$

meridians = [PolyLine [(perspective . cartesian)
  (Spheric3D (DEG l) (DEG d))
  | d <- delta]
    | l <- lambda]
  where
    delta = [-90,-80..90]
    lambda = [-90,-75..90]

latitudes = [PolyLine [(perspective . cartesian)
  (Spheric3D (DEG l) (DEG d))
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



layers1 = [
  Phantom (Point (-s) (-s)) (Point s s),
  --Layer "1" Red [Line p1 p2] "[line width=0.8pt]",
  Layer "1" Black axes "[line width=0.8pt]",
  Layer "1" Black latitudes "[line width=0.8pt]",
  Layer "2" Black meridians "[line width=0.8pt]"]

tpict = tikzPicture layers1

main = do
  putStrLn tpict




