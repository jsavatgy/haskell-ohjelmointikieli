import Eemian

p0 = Point 0 0
p1 = Point 1 0
p2 = Point 0 1

c1 = Circle 1.0 p1


line1 = Line (Point 4.1 (-0.1)) (Point 4.1 4.1)

arrows1 = 
  [Arrow "" p0 p1] ++
  [Arrow "" p0 p2] 
pts1 =
  [NamedSymbPos "$(0,0)$" "$\\bullet$" SE (Node p0)] ++
  [NamedSymbPos "$(1,0)$" "$\\bullet$" SW (Node p1)] ++
  [NamedSymbPos "$(0,1)$" "$\\bullet$" NW (Node p2)]
  --[Node p3, Node p4] ++
  --[TextE p3 "$p_3$" "above left"] ++
  --[TextE p4 "$p_4$" "below right"] ++
circles1 = 
  [c1]

ay1 = Point 0 (-1.2)
ay2 = Point 0 1.2
ax1 = Point (-1.2) 0
ax2 = Point 1.2 0

axes = 
  [Arrow "" ax1 ax2] ++
  [TextE ay2 "$y$" "below right"] ++
  [Arrow "" ay1 ay2] ++
  [TextE ax2 "$x$" "above left"] 

layers1 = [
  Phantom (Point (-10) (-1)) (Point 10 5),
  --Layer "0" White [line1] "",
  Layer "1" Black axes "[line width=0.8pt]",
  --Layer "1" Black arrows1 "[line width=0.8pt]",
  Layer "1" Black pts1 "[line width=0.8pt]",
  Layer "1" Black circles1 "[line width=0.8pt,dashed]" ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict


