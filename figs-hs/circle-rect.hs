
import Eemian

circle1 = Circle 1 (Point 0 0)
r1 = [shorten 0.12 0.00 p1 p2]
  where
    p1 = Point 0 0
    p2 = Point 1 0

r2 = [
  Filled (Circle 0.075 p1),
  TextE tp "$r$" "above"
  ]
  where
    tp = midway p1 p2
    p1 = Point 0 0
    p2 = Point 1 0

rect1 = [PolyLine [p1,p2,p3,p4,p1],
  TextE a "$a$" "above",
  TextE b "$b$" "left" ]
  where
    a = midway p1 p2
    b = midway p2 p3
    p1 = Point 2 (-1)
    p2 = Point 5 (-1)
    p3 = Point 5 1
    p4 = Point 2 1

layers1 = [
  Phantom (Point (-7) (-2)) (Point 7 (-2)),
  Layer "1" Black [circle1] "[line width=0.8pt]", 
  Layer "1" Black r2 "[line width=0.8pt]", 
  Layer "3" Black r1 "[line width=0.8pt,dashed]", 
  Layer "2" Black rect1 "[line width=0.8pt]" ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict

