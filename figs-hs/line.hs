import Eemian

line1 = Line (Point 0 0) (Point 2 2)
circle1 = Circle 1.8 (Point 2 2)
ys = cycle [3.5,2.5]
xs = [0,0.5..5]
polyline1 = PolyLine [Point x y | (x,y) <- zip xs ys]

whiteLine = Line (Point 5.1 (-0.6)) (Point 5.1 5.1)

shapes1 = 
  [line1,circle1,polyline1]

axes = 
  [Arrow "" (Point (-0.5) 0) (Point 5 0)] ++
  [TextE (Point 5 0) "$x$" "above left"] ++
  [Arrow "" (Point 0 (-0.5)) (Point 0 5)] ++
  [TextE (Point 0 5) "$y$" "below right"] 

layers1 = [
  Phantom (Point (-10) (-1)) (Point 10 5),
  Layer "0" White [whiteLine] "",
  Layer "1" Black shapes1 "[line width=0.8pt]",
  Layer "1" Black axes "[line width=0.8pt]" ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict


