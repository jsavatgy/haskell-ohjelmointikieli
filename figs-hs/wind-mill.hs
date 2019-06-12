
import Eemian



rotationMatrix1 t = [[cos1 t,-sin1 t],[sin1 t,cos1 t]]

matrixTimes1 a b = 
  [sum [x * y | (x,y) <- zip a1 b] | a1 <- a]


rot1 t (Point x1 y1) = Point x y
  where
    [x,y] = matrixTimes1 (rotationMatrix1 t) [x1,y1]

t1 = twopi / 3

blade alpha = Polygon pts2
  where
    pts2 = map (rot1 alpha) pts1
    pts1 = [Point 0 0,Point 0.7 (-0.1),Point 0.3 0.1]

tower = Polygon [p1,p2,p3,p4]
  where
    p1 = Point (-0.02) 0
    p2 = Point(-0.07) (-1)
    p3 = Point 0.07 (-1)
    p4 = Point 0.02 0

rotor = Circle 0.05 (Point 0 0)

windMill = [blade (RAD alpha) | alpha <- [0,t1,2*t1]] ++
  [rotor] ++ [tower]

c1 = Circle 1.0 p0


line1 = Line (Point 4.1 (-0.1)) (Point 4.1 4.1)

pts1 =
  [Named "1" (Line p0 p1)]
  --[NamedSymbPos "$p_0$" "$\\bullet$" SW (Node p0)] ++
  -- [NamedSymbPos "$\\texttt{RAD}\\,\\frac{\\pi}{2}$" "" NE (Node p0)] ++
  --[NamedSymbPos "$p_1$" "$\\bullet$" NE (Node p1)] 
  --[Node p3, Node p4] ++
  --[TextE p3 "$p_3$" "above left"] ++
  --[TextE p4 "$p_4$" "below right"] ++
circles1 = 
  [c1]


ay1 = Point 0 (-1.15)
ay2 = Point 0 1.2
ax1 = Point (-1.15) 0
ax2 = Point 1.2 0

axes = 
  [Arrow "" ax1 ax2] ++
  [TextE ay2 "$y$" "below right"] ++
  [Arrow "" ay1 ay2] ++
  [TextE ax2 "$x$" "above left"] 

layers1 = [
  Phantom (Point (-3) (-1.2)) (Point 3 1.2),
  --Layer "0" White [line1] "",
  Layer "1" Black axes "[line width=0.8pt]",
  --Layer "1" Black arrows1 "[line width=0.8pt]",
  Layer "1" Black windMill "[line width=1.6pt,line join=round,line cap=round]",
  Layer "1" Black circles1 "[line width=0.8pt,dashed]" ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict



