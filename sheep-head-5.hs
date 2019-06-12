import Eemian
import Data.List.Split (chunksOf)
import Data.List (zip4,intercalate)
import Text.Printf (printf)


-- https://en.wikipedia.org/wiki/B%C3%A9zier_curve
bezier p0 p1 p2 p3 t = foldr1 move0 [
  ((1 - t) ** 3) `scale0` p0,
  (3 * (1 - t) ** 2 * t) `scale0` p1,
  (3 * (1 - t) * t ** 2) `scale0` p2,
  (t ** 3) `scale0` p3 ]


sheepHead1 = [mkPoint (bezier p0 p1 p2 p3 t) 
  | (p0,p1,p2,p3) <- headPts1, t <- [0.0,0.1..0.9]]

headPts1 = zip4 sc1 ex1 ex2 (tail sc1)
  where
   ex2 = [move0 a b | (a,b) <- (zip sc1 ext2)]
   ex1 = [move0 a b | (a,b) <- (zip sc1 ext1)]
   ext2 = [p1 !! 1 | p1 <- pts2]
   ext1 = [p1 !! 0 | p1 <- pts2]
   sc1 = scanl move0 (2,0) pts3
   pts3 = [p1 !! 2 | p1 <- pts2]
   pts2 = chunksOf 3 pts1
   pts1 = map (\(x,y) -> (x,-y)) pts0


pts0 = [ 
  (-0.37,-1.28),(-1.09,-2.34),(-1.82,-2.70),
  (-0.50,-0.24),(-1.28,-0.24),(-1.69,0.01),
  (-0.63,0.39),(-1.01,1.22),(-1.00,2.18),
  (0.01,1.31),(0.71,2.29),(1.89,2.68),
  (0.52,0.17),(0.62,0.19),(1.16,0.20),
  (0.40,0.01),(0.56,-0.03),(1.00,-0.29),
  (0.36,-0.22),(0.71,-1.20),(0.45,-2.09) ]

sheepHead2 = [Filled (Polygon sheepHead1)]

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
  Phantom (Point (-10) (-5)) (Point 10 5),
  --Layer "1" Black axes "[line width=0.8pt]",
  --Layer "1" Black [Node (Point 0 0)] "[line width=0.8pt]",
  --Layer "1" Black namedPts2 "[line width=0.8pt]",
  Layer "1" Gray30 sheepHead2 "[line width=0.8pt]"
  --Layer "1" Black circles1 "[line width=0.8pt]"
  --Layer "1" Black arcs1 "[line width=0.8pt]"
  ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict



