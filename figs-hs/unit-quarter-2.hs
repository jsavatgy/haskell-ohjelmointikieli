import Eemian

p0 = Point 0 0

t1 = RAD (halfpi / 3)
p1 = pointFromPolar t1 1

t5 = RAD 0.20
p5 = pointFromPolar t5 1

t6 = RAD 0.245
p6 = pointFromPolar t6 1


c1 = Circle 1.0 p0
arc1 = Arc 1.0 p0 (GON (-10)) (GON 110)
arc2 = Arc 1.0 p0 (DEG 0) t5

line1 = Line (Point 4.1 (-0.1)) (Point 4.1 4.1)

arrow1 = 
  [Arrow "" p5 p6] ++
  [arc2] 

fig1 =
  [Named "$1$" (Line p0 p1)] ++
  --[NamedSymbPos "$p_0$" "$\\bullet$" SE (Node p0)] ++
  [NamedSymbPos "$\\mathbf p$" "$\\bullet$" NE (Node p1)] ++
  --[NamedSymbPos "$\\theta$" "" NE (Node p0)] ++

  [NamedSymbPos "$\\theta$" "" NE (Node p5)]
  --[Node p3, Node p4] ++
  --[TextE p3 "$p_3$" "above left"] ++
  --[TextE p4 "$p_4$" "below right"] ++
circles1 = 
  [c1]

(x1,y1) = (Point x sinM10, Point sinM10 y)
  where
    Point x y = p1

(x2,y2) = (Point x 1.2, Point 1.2 y)
  where
    Point x y = p1

(x3,y3) = (Point x 0, Point 0 y)
  where
    Point x y = p1
x33 = midway p0 x3
y33 = midway p0 y3


sinM10 = sin1 (GON (-10))
ay1 = Point 0 sinM10
ay2 = Point 0 1.2
ax1 = Point sinM10 0
ax2 = Point 1.2 0

axes = 
  [Arrow "" ax1 ax2] ++
  --[TextE ay2 "$y$" "below right"] ++
  [TextE y33 "$y$" "left"] ++
  [Arrow "" ay1 ay2] 
  -- ++ [TextE ax2 "$x$" "above left"] 
  ++ [TextE x33 "$x$" "below"] 

dashed = [arc1] 
  ++ [Line x1 x2]
  ++ [Line y1 y2]
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


