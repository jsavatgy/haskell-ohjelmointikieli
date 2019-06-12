import Eemian

pointOfEllipse a b t = Point x y 
  where
    x = a * cos t
    y = b * sin t

pls = [PolyLine (map (`addCoords` Point (1.5 * x) 0)
  (pts1 dv)) 
  | (x,dv) <- zip [1..] [10,15..35]]

pts1 n = [pointOfEllipse 3 2 t | t <- [0,dt..twopi]]
  where
    dt = twopi / n

layers1 = [
  Phantom (Point (-8) (-1)) (Point 8 5),
  Layer "1" Black pls "[line width=0.8pt]" 
  ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict


