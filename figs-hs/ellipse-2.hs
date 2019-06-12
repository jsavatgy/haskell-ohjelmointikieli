import Eemian

pointOfEllipse a b t = Point x y 
  where
    x = a * cos t
    y = b * sin t

dotsEllipse = [Filled (Circle 0.12 p) | p <- pts]
  where
    pts = [alongPL pl1 (s * l2) | s <- [1..n]]
    l2 = l1 / n
    l1 = lengthPL pl1
    n = 15

pl1 = PolyLine pts

pts = [pointOfEllipse 3 2 t | t <- [0,dt..twopi]]
  where
    dt = twopi / n
    n = 120

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
  Layer "1" Black dotsEllipse "[line width=0.8pt]"]

tpict = tikzPicture layers1

main = do
  putStrLn tpict


