import Eemian


t1 = RAD 2.4409158645110915
t2 = RAD (-1.7508052163499301)
p0 = Point 0 0
p1 = pointFromPolar t1 1
p2 = pointFromPolar t2 1

c1 = Circle 1.0 p0


line1 = Line (Point 4.1 (-0.1)) (Point 4.1 4.1)

pts1 =
  [Line p0 p1] ++
  [Line p0 p2] ++
  [NamedSymbPos "$p_0$" "$\\bullet$" NE (Node p0)] ++
  -- [NamedSymbPos "$\\texttt{RAD}\\,\\frac{\\pi}{2}$" "" NE (Node p0)] ++
  [NamedSymbPos "$p_1$" "$\\bullet$" NW (Node p1)] ++
  [NamedSymbPos "$p_2$" "$\\bullet$" SW (Node p2)]
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
  Phantom (Point (-3.5) (-1.2)) (Point 3.5 1.2),
  --Layer "0" White [line1] "",
  Layer "1" Black axes "[line width=0.8pt]",
  --Layer "1" Black arrows1 "[line width=0.8pt]",
  Layer "1" Black pts1 "[line width=0.8pt]",
  Layer "1" Black circles1 "[line width=0.8pt,dashed]" ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict


