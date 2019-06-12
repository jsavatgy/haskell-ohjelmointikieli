
import Eemian

xs = [3,5,7,11]
rects1 = concat [[
  rect x 0, 
  lnk x 0, 
  bullet x 0, 
  arrow x 0,
  Text (Point x 0) (show str)]
  | (str,x) <- zip xs [1..]] 

head1 = [Texttt p1 "xs" "left",
  Arrow "" p1 p2]
  where
    p2 = Point p2x y
    p1 = Point p1x y
    p2x = (x + 1 - r1 - r3)
    p1x = x + 3 * r1
    (x,y) = (0,0)

ground1 = ground 5 0

ground x y =  [fromRect (Rect p1 p3),
  shorten 0.01 0.01 p1 p3,
  shorten 0.01 0.01 p2 p4]
  where
    p1 = Point (x-r1) (y-r2)
    p2 = Point (x+r1) (y-r2)
    p3 = Point (x+r1) (y+r2)
    p4 = Point (x-r1) (y+r2)


lnk x y = PolyLine [p1,p2,p3,p4]
  where
    p1 = Point p1x (y-r2)
    p2 = Point p2x (y-r2)
    p3 = Point p2x (y+r2)
    p4 = Point p1x (y+r2)
    p2x = p1x + 2 * r1
    p1x = x+r1

bullet x y = --Text p "$\\bullet$"
  Filled (Circle 0.045 p)
  where
    p = Point p1x y
    p1x = x + 2 * r1

arrow x y = Arrow "" p1 p2
  where
    p2 = Point p2x y
    p1 = Point p1x y
    p2x = (x + 1 - r1 - r3)
    p1x = x + 2 * r1

(r1,r2,r3) = (0.15,0.175,0.015)

rect x y = fromRect (Rect p1 p2)
  where
    p1 = Point (x-r1) (y-r2)
    p2 = Point (x+r1) (y+r2)

layers1 = [
  Phantom (Point (-3) (-2)) (Point 3 2),
  Layer "1" Black head1 "[line width=0.8pt]",
  Layer "2" Black ground1 "[line width=0.8pt]",
  Layer "3" Black rects1 "[line width=0.8pt]" ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict

