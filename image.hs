
import Eemian

circle1 = Circle 1 (Point 0 0)

rect1 = PolyLine [p1,p2,p3,p4,p1]
  where
    p1 = Point 2 (-1)
    p2 = Point 5 (-1)
    p3 = Point 5 1
    p4 = Point 2 1

layers1 = [
  Phantom (Point (-3) (-2)) (Point 3 (-2)),
  Layer "1" Black [circle1] "[line width=0.8pt]", 
  Layer "2" Black [rect1] "[line width=0.8pt]" ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict

