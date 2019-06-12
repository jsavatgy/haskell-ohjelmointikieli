import Eemian

pointOfEllipse a b t = Point x y 
  where
    x = a * cos t
    y = b * sin t

pl1 = PolyLine (map (`subCoords` p1) (pts1 20))
pl2 = PolyLine (map (`addCoords` p1) (pts1 120)) 
p1 = Point 3.5 0

pts1 n = [pointOfEllipse 3 2 t | t <- [0,dt..twopi]]
  where
    dt = twopi / n

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
  Layer "1" Black [pl1] "[line width=0.8pt]",
  Layer "2" Black [pl2] "[line width=0.8pt]"]

tpict = tikzPicture layers1

main = do
  putStrLn tpict


