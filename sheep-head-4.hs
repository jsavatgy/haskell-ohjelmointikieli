import Eemian
import Data.List.Split (chunksOf)
import Data.List (zip4)


-- https://en.wikipedia.org/wiki/B%C3%A9zier_curve
bezier p0 p1 p2 p3 t = foldr1 move0 [
  ((1 - t) ** 3) `scale0` p0,
  (3 * (1 - t) ** 2 * t) `scale0` p1,
  (3 * (1 - t) * t ** 2) `scale0` p2,
  (t ** 3) `scale0` p3 ]


sheepHead1 = [mkPoint (bezier p0 p1 p2 p3 t) 
  | (p0,p1,p2,p3) <- headPts1, t <- [0.0,0.1..0.9]]

headPts1 = zip4 sc1 ex1 ex2 (tail (sc1 ++ [(0,0)]))
  where
   ex2 = [move0 a b | (a,b) <- (zip sc1 ext2)]
   ex1 = [move0 a b | (a,b) <- (zip sc1 ext1)]
   ext2 = [p1 !! 1 | p1 <- pts2]
   ext1 = [p1 !! 0 | p1 <- pts2]
   sc1 = scanl move0 (0,0) pts3
   pts3 = [p1 !! 2 | p1 <- pts2]
   pts2 = chunksOf 3 pts1
   pts1 = map (\(x,y) -> (x,-y)) pts0
   pts0 = [ 
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


--pts1 = [Filled (Circle 0.15 p) | p <- sheepHead1]
sheepHead2 = [Polygon sheepHead1]

layers1 = [
  Phantom (Point (-10) (-1)) (Point 10 5),
  --Layer "1" Black arcs1 "[line width=0.8pt]",
  --Layer "1" Black pts1 "[line width=0.8pt]"
  Layer "1" Black sheepHead2 "[line width=0.8pt]"
  --Layer "1" Black circles1 "[line width=0.8pt]"
  --Layer "1" Black arcs1 "[line width=0.8pt]"
  ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict



