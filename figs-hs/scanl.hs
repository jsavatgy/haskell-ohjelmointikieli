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

sheepHead1 = [mkPoint (bezier p0 p1 p2 p3 t) 
  | (p0,p1,p2,p3) <- headPts1, t <- [0.0,0.1..0.9]]

headPts1 = zip4 sc1 ex1 ex2 (tail sc1)
  where
   ex2 = [move0 a b | (a,b) <- (zip sc1 ext2)]
   ex1 = [move0 a b | (a,b) <- (zip sc1 ext1)]
   ext2 = [p1 !! 1 | p1 <- pts2]
   ext1 = [p1 !! 0 | p1 <- pts2]
   sc1 = scanl move0 (30,15) pts3
   pts3 = [p1 !! 2 | p1 <- pts2]
   pts2 = chunksOf 3 pts0
   --pts1 = map (\(x,y) -> (x,-y)) pts0

namedPts3 = [NamedSymbPos ("$p_" ++ show n ++ "$") 
  "$\\bullet$" SE (Node (mkPoint p0)) 
  | (n,p0) <- zip [1..] pts5]
  
pts5 = scanl1 move0 pts4

namedPts2 = init [NamedSymbPos ("$p_" ++ show n ++ "$") 
  "$\\bullet$" SE (Node (mkPoint p0)) 
  | (n,p0) <- zip [1..] (nub pts)]
  where
    pts = concat [[p0,p3] | (p0,p1,p2,p3) <- headPts1]

showTuple02 pts = intercalate "," 
  ["(" ++ op1 a ++ "," ++ op1 b ++ ")" 
   | (a,b) <- pts]
  where
    op1 x = show2 (x/3)

pts4 = [ (30.00,15.00), (-55.08,28.87), (-14.71,-65),
  (69.04,-24.41)]

pts0 = [
 (-12.75,26.54), (-35.17,37.72), (-55.08,28.87), 
 (-24.64,-13.44), (-23.61,-46.86), (-14.71,-65.56),
 (11.01,-22.87), (46.80,-38.26), (69.04,-24.41),
 (16.65,14.17), (10.60,40.59), (0.74,61.10) ]

{-
pts0 = [ 
  (9.85,-20.50), (15.91,-46.92), (-0.74,-61.10), 
  (-22.24,-13.84), (-58.03,1.53), (-69.04,24.41),
  (-8.90,18.69), (-9.92,52.11), (14.71,65.56),
  (19.90,8.85), (42.32,-2.32), (55.08,-28.87) ]

"m 224.06,140.75 c -12.75,26.54 -35.17,37.72 -55.08,28.87 
 -24.64,-13.44 -23.61,-46.86 -14.71,-65.56 11.01,-22.87 
 46.80,-38.26 69.04,-24.41 16.65,14.17 10.60,40.59 
 0.74,61.10 z"

inverted:
"m 224.06,140.75 c 9.85,-20.50 15.91,-46.92 -0.74,-61.10 
 -22.24,-13.84 -58.03,1.53 -69.04,24.41 -8.90,18.69 
 -9.92,52.11 14.71,65.56 19.90,8.85 42.32,-2.32 
  55.08,-28.87 z" -}

sheepHead2 = [Polygon sheepHead1]

ay1 = Point 0 (-60)
ay2 = Point 0 60
ax1 = Point (-60) 0
ax2 = Point 60 0

axes = 
  [Arrow "" ax1 ax2] ++
  [TextE ay2 "$y$" "below right"] ++
  [Arrow "" ay1 ay2] ++
  [TextE ax2 "$x$" "above left"] 

layers1 = [
  Phantom (Point (-170) (-50)) (Point 170 50),
  Layer "1" Black axes "[line width=0.8pt]",
  --Layer "1" Black [Node (Point 0 0)] "[line width=0.8pt]",
  Layer "1" Black namedPts3 "[line width=0.8pt]"
  --Layer "1" Black sheepHead2 "[line width=0.8pt]"
  --Layer "1" Black circles1 "[line width=0.8pt]"
  --Layer "1" Black arcs1 "[line width=0.8pt]"
  ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict



