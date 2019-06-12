import Eemian

p0 = Point 0 0

t1 = RAD 0.2035000863508146
p1 = pointFromPolar t1 1

t5 = RAD 0.13
p5 = pointFromPolar t5 1

t6 = RAD 0.20
p6 = pointFromPolar t6 1


c1 = Circle 1.0 p0
arc1 = Arc 1.0 p0 (GON (-10)) (GON 110)
arc2 = Arc 1.0 p0 (DEG 0) t6

line1 = Line (Point 4.1 (-0.1)) (Point 4.1 4.1)

arrow1 = 
  --[Arrow "" p5 p6] ++
  [arc2] 

fig1 =
  [Named "1737.1 km" (Line p0 p1)] ++
  --[NamedSymbPos "$p_0$" "$\\bullet$" SE (Node p0)] ++
  [NamedSymbPos "353.5 km" "$\\bullet$" E (Node p1)] 
  -- ++ [NamedSymbPos "353.5 km" "" E (Node p5)]
  --[Node p3, Node p4] ++
  --[TextE p3 "$p_3$" "above left"] ++
  --[TextE p4 "$p_4$" "below right"] ++
circles1 = 
  [c1]

sinM10 = sin1 (GON (-10))
ay1 = Point 0 sinM10
ay2 = Point 0 1.2
ax1 = Point sinM10 0
ax2 = Point 1.2 0

axes = 
  [Arrow "" ax1 ax2] ++
  [TextE ay2 "$y$" "below right"] ++
  [Arrow "" ay1 ay2] ++
  [TextE ax2 "$x$" "below left"] 

dashed = [arc1] 
  -- ++ [Line ax3 ax4]
  where
    ax3 = Point sinM10 0.5
    ax4 = Point 1.2 0.5

layers1 = [
  Phantom (Point (-3) (-1.2)) (Point 3 1.2),
  --Layer "0" White [line1] "",
  Layer "1" Black axes "[line width=0.8pt]",
  --Layer "1" Black arrows1 "[line width=0.8pt]",
  Layer "1" Black arrow1 "[line width=1.0pt]",
  Layer "1" Black fig1 "[line width=0.8pt]",
  Layer "1" Black dashed "[line width=0.8pt,dashed]" ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict


