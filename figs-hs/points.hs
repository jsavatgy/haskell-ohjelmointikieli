import Eemian

p1 = Point 1 1
p2 = Point 3 2

line1 = Line (Point 4.1 (-0.1)) (Point 4.1 4.1)

points1 = 
  [NamedSymbPos "$p_1$" "$\\bullet$" SE (Node p1)] ++
  [NamedSymbPos "$p_2$" "$\\bullet$" SE (Node p2)] ++
  [Arrow "" (Point 0 0) (Point 4 0)] ++
  [TextE (Point 4 0) "$x$" "above left"] ++
  [Arrow "" (Point 0 0) (Point 0 4)] ++
  [TextE (Point 0 4) "$y$" "below right"] 

layers1 = [
  Phantom (Point (-10) (-1)) (Point 10 5),
  Layer "0" White [line1] "",
  Layer "1" Black points1 "[line width=0.8pt]" ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict


