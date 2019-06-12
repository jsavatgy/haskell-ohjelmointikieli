import Eemian
import Data.List.Split (chunksOf)
import Data.List (zip4,intercalate,nub)
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
  (-5.10,-4.20), (-10.63,-1.91), (-11.31,0.94), 
  (-0.54,2.26), (3.02,3.16), (4.79,1.98), 
  (1.97,-1.31), (4.06,-1.91), (5.62,-1.11) ]

earL = [
  (4.44,-4.29), (10.22,-1.96), (10.84,0.11),
  (0.61,2.07), (-0.51,3.04), (-1.88,3.02),
  (-1.37,-0.02), (-5.81,-2.55), (-6.92,-1.44) ]

sheepEars2 = [PolyLine (sheepEar earR)]
  --,PolyLine (sheepEar earL)]

namedPts2 = [NamedSymbPos "" "$\\bullet$" E
  (Node (mkPoint p0)) | p0 <- nub pts]
  where
    pts = concat [[p0,p3] | (p0,p1,p2,p3) <- earPts1 earR]

ay1 = Point 0 (-2.8)
ay2 = Point 0 3.7
ax1 = Point (-3.0) 0
ax2 = Point 3.0 0

axes = 
  [Arrow "" ax1 ax2] ++
  [TextE ay2 "$y$" "below right"] ++
  [Arrow "" ay1 ay2] ++
  [TextE ax2 "$x$" "above left"] 

layers1 = [
  Phantom (Point (-20) (-5)) (Point 20 5),
  --Layer "1" Black axes "[line width=0.8pt]",
  Layer "1" Black namedPts2 "[line width=0.8pt]",
  Layer "1" Black sheepEars2 "[line width=0.8pt]"
  ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict



