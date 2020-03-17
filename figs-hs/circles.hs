import Eemian

p1 = Point 1 1
p2 = Point 3 2

c2 = Circle 2.5 p1
c3 = Circle 2.5 p2

[p3,p4] = circleCircleIntersections c2 c3


{-
-}
line1 = Line (Point 4.1 (-0.1)) (Point 4.1 4.1)

circles1 =
  [NamedSymbPos "$\\mathbf p_3$" "$\\bullet$" NW (Node p3)] ++
  [NamedSymbPos "$\\mathbf p_4$" "$\\bullet$" SE (Node p4)] ++
  --[Node p3, Node p4] ++
  --[TextE p3 "$p_3$" "above left"] ++
  --[TextE p4 "$p_4$" "below right"] ++
  [c2,c3] ++
  axes

ay1 = Point 0 (-2)
ay2 = Point 0 5
ax1 = Point (-2) 0
ax2 = Point 6 0

axes = 
  [Arrow "" ax1 ax2] ++
  [TextE ay2 "$y$" "below right"] ++
  [Arrow "" ay1 ay2] ++
  [TextE ax2 "$x$" "above left"] 

layers1 = [
  Phantom (Point (-10) (-1)) (Point 10 5),
  --Layer "0" White [line1] "",
  Layer "1" Black circles1 "[line width=0.8pt]" ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict


