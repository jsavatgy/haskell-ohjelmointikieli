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

pts0B = [ 
    (-1.106451,-3.83916),
    (-3.259703,-7.02543),
    (-5.471211,-8.09601),
    (-1.51227,-0.73207),
    (-3.851271,-0.71483),
    (-5.068354,0.0374),
    (-1.885143,1.16507),
    (-3.018469,3.64683),
    (-2.993332,6.55479),
    (0.0339,3.92141),
    (2.117241,6.86883),
    (5.68336,8.04051),
    (1.565461,0.51435),
    (1.852023,0.563),
    (3.49038,0.59264),
    (1.208068,0.0219),
    (1.694509,-0.0864),
    (3.013445,-0.85934),
    (1.068091,-0.66145),
    (2.118171,-3.58972),
    (1.345712,-6.26999)]

namedPts1 = [NamedSymbPos ("$p_" ++ show n ++ "$") "$\\bullet$" d1
  (Node (mkPoint p)) 
  | (n,p,d1) <- zip3 [1..] sc1 [SW,NE,NE,SE,SW,SE,SE]]

namedPts2 = [NamedSymbPos "" "$\\bullet$" E
  (Node (mkPoint p3)) 
  | (p0,p1,p2,p3) <- headPts1]



pts3 = map (\(x,y) -> (x,-y)) pts2

pts2 = [(2,0),(-1.82,-2.70),(-1.69,0.01),(-1.00,2.18),
  (1.89,2.68),(1.16,0.20),(1.00,-0.29)]

sc1 = scanl1 move0 pts3

pts6 = [(0,0),(-5.47,-8.10),(-5.07,0.04),(-2.99,6.55),
  (5.68,8.04),(3.49,0.59),(3.01,-0.86)]

pts5 = [(-5.47,-8.10),(-5.07,0.04),(-2.99,6.55),
  (5.68,8.04),(3.49,0.59),(3.01,-0.86),(1.35,-6.27)]

showTuple02 pts = intercalate "," 
  ["(" ++ op1 a ++ "," ++ op1 b ++ ")" 
   | (a,b) <- pts]
  where
    op1 x = show2 (x/3)

pts0 = [ 
  (-0.37,-1.28),(-1.09,-2.34),(-1.82,-2.70),
  (-0.50,-0.24),(-1.28,-0.24),(-1.69,0.01),
  (-0.63,0.39),(-1.01,1.22),(-1.00,2.18),
  (0.01,1.31),(0.71,2.29),(1.89,2.68),
  (0.52,0.17),(0.62,0.19),(1.16,0.20),
  (0.40,0.01),(0.56,-0.03),(1.00,-0.29),
  (0.36,-0.22),(0.71,-1.20),(0.45,-2.09) ]

pts0BB = [ 
  (-1.11,-3.84),(-3.26,-7.03),(-5.47,-8.10),
  (-1.51,-0.73),(-3.85,-0.71),(-5.07,0.04),
  (-1.89,1.17), (-3.02,3.65), (-2.99,6.55),
  (0.03,3.92),  (2.12,6.87),  (5.68,8.04),
  (1.57,0.51),  (1.85,0.56),  (3.49,0.59),
  (1.21,0.02),  (1.69,-0.09), (3.01,-0.86),
  (1.07,-0.66), (2.12,-3.59), (1.35,-6.27) ]

{- sheep-head-2.svg
       d="m 71.377576,152.61966 c -1.106451,-3.83916 -3.259703,-7.02543 -5.471211,-8.09601 -1.51227,-0.73207 -3.851271,-0.71483 -5.068354,0.0374 -1.885143,1.16507 -3.018469,3.64683 -2.993332,6.55479 0.0339,3.92141 2.117241,6.86883 5.68336,8.04051 1.565461,0.51435 1.852023,0.563 3.49038,0.59264 1.208068,0.0219 1.694509,-0.0864 3.013445,-0.85934 1.068091,-0.66145 2.118171,-3.58972 1.345712,-6.26999 z"
-}

pts02 = intercalate "," 
  ["(" ++ show2 a ++ "," ++ show2 b ++ ")" 
   | (a,b) <- pts0]

pts03 = intercalate "," 
  ["(" ++ show3 a ++ "," ++ show3 b ++ ")" 
   | (a,b) <- pts0]

--pts1 = [Filled (Circle 0.15 p) | p <- sheepHead1]
sheepHead2 = [Polygon sheepHead1]

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
  Layer "1" Black axes "[line width=0.8pt]",
  --Layer "1" Black [Node (Point 0 0)] "[line width=0.8pt]",
  Layer "1" Black namedPts2 "[line width=0.8pt]",
  Layer "1" Black sheepHead2 "[line width=0.8pt]"
  --Layer "1" Black circles1 "[line width=0.8pt]"
  --Layer "1" Black arcs1 "[line width=0.8pt]"
  ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict



