import Eemian

p1 = Point 1 1
p2 = Point 3 2
p5 = Point 3.5  1

c1 = Circle 2.5 p1
c2 = Circle 2.5 p2

[p3,p4] = circleCircleIntersections c1 c2

line1 = Line (Point 4.1 (-0.1)) (Point 4.1 4.1)

lines1 = 
  [Line p1 p3] ++
  [Line p1 p4] ++
  [Line p1 p5] ++
  [NamedSymbPos "$\\mathbf v_0$" "" N (Node (halfway p1 p5))] ++
  [NamedSymbPos "$\\mathbf v_1$" "" E (Node (halfway p1 p3))] ++
  [NamedSymbPos "$\\mathbf v_2$" "" SW (Node (halfway p1 p4))]
  -- [Line p2 p3] ++
  -- [Line p2 p4] 
  
pts1 =
  [NamedSymbPos "$\\mathbf p_1$" "$\\bullet$" SW (Node p1)] ++
  [NamedSymbPos "$\\mathbf p_3$" "$\\bullet$" NW (Node p3)] ++
  [NamedSymbPos "$\\mathbf p_4$" "$\\bullet$" SE (Node p4)] ++
  [NamedSymbPos "$\\mathbf p_5$" "$\\bullet$" SE (Node p5)] 
  --[Node p3, Node p4] ++
  --[TextE p3 "$p_3$" "above left"] ++
  --[TextE p4 "$p_4$" "below right"] ++

circles1 = 
  [c1]

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
  --Layer "1" Black axes "[line width=0.8pt]",
  Layer "1" Black lines1 "[line width=0.8pt]",
  --Layer "1" Black arrows1 "[line width=0.8pt]",
  Layer "1" Black pts1 "[line width=0.8pt]",
  Layer "1" Black circles1 "[line width=0.8pt,dashed]" ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict


