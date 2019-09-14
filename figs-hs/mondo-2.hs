import Data.List.Split
import Data.List
import Data.Maybe
import Data.Char
import Eemian

coastFile = "coastline/gshhs_c.txt"


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

matr1 pv pAlpha (Point3D x1 y1 z1) = Point x y
  where
    [x,y,z] = matrixTimes3 (matrix1 pv alpha) [x1,y1,z1]
    alpha = DEG pAlpha

matrixTimes3 a b = 
  [sum [x * y | (x,y) <- zip a1 b] | a1 <- a]

r = 100 

orthoYZ (Point3D x y z) = Point y z

perspective = matr1 pv pAlpha
  where
    pv = 2
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
    delta = [-90,-75..90]
    lambda = [-180,-165..180]

latitudes = [PolyLine [(perspective . cartesian)
  (GeographicNE (DEG d) (DEG l))
  | l <- lambda]
    | d <- delta]
  where
    delta = [-90,-75..90]
    lambda = [-180,-160..180]

simplify (x:y:zs)
  | dist x y < 0.8 = simplify (x:zs)
  | otherwise    = x : y : simplify zs
simplify xs = xs

untab str = splitOn "\t" str

untabPts str = GeographicNE (DEG y) (DEG x)
  where
    [x,y] = map readd [a,b]
    [a,b] = untab str
    readd x = read x :: Double

pgPts xs = map untabPts xs

isCommentEtc str =
  "#" `isPrefixOf` str || ">" `isPrefixOf` str

s = 300

layers1 pgs = [
  Phantom (Point (-s) (-s)) (Point s s),
  Layer "1" Black pgs "[line width=0.5pt,line join=round,line cap=round]",
  Layer "3" Black latitudes "[line width=0.3pt]",
  Layer "4" Black meridians "[line width=0.3pt]",
  Empty
  ]

tpict c = tikzPicture (layers1 c)

main = do
  content <- readFile coastFile
  let 
    x1 = splitWhen isCommentEtc (lines content)
    x2 = filter (not . null) x1
    x3 = map pgPts x2
    x4 = [map (perspective . cartesian) pts | pts <- x3]
    x5 = map simplify x4
    x6 = map PolyLine x5
  putStrLn (tpict x6)

