
import Eemian
import Data.Char (isLetter)

data Tree a = Empty | Branch a (Tree a) (Tree a)

tree =
  Branch 'N' (Branch 'I' (Branch 'E' (Branch 'C' (Branch 'A' (Branch ' ' Empty Empty) (Branch 'B' Empty Empty)) (Branch 'D' Empty Empty)) (Branch 'G' (Branch 'F' Empty Empty) (Branch 'H' Empty Empty))) (Branch 'L' (Branch 'K' (Branch 'J' Empty Empty) (Branch ' ' Empty Empty)) (Branch 'M' Empty Empty))) (Branch 'Y' (Branch 'S' (Branch 'P' (Branch 'O' Empty Empty) (Branch 'R' (Branch 'Q' Empty Empty) (Branch ' ' Empty Empty))) (Branch 'U' (Branch 'T' Empty Empty) (Branch 'W' (Branch 'V' Empty Empty) (Branch 'X' Empty Empty)))) (Branch '\201' (Branch '\196' (Branch 'Z' (Branch ' ' Empty Empty) (Branch '\194' Empty Empty)) (Branch '\200' (Branch '\198' Empty Empty) (Branch ' ' Empty Empty))) (Branch '\214' (Branch '\202' Empty Empty) (Branch ' ' Empty Empty))))

paint p (Empty) = [] 
paint p (Branch ' ' tree1 tree2) = []
paint p (Branch str tree1 tree2) = 
  paintBranch p [str] tree1 tree2

paintBranch p str tree1 tree2 =
  [Texttt (real p) str ""] ++
  (if (valid tree1) then [shorten r2 r2 (real p) (real leftHand)] else []) ++ 
  (if (valid tree2) then [shorten r2 r2 (real p) (real rightHand)]  else []) ++
  paint leftHand tree1 ++
  paint rightHand tree2
  where
    leftHand  = Point (x - (1/2**(y+2))) (y+1)
    rightHand = Point (x + (1/2**(y+2))) (y+1)
    real (Point x y) = Point x (-0.07 * y)
    Point x y = p
    r2 = 0.02

valid (Branch letter tree1 tree2) = isLetter letter
valid (Empty) = False

mPoint x y = Point x (-y)

tree1 = paint (Point 0 0) tree

depth (Branch x left right) = 
  1 + (depth left `max` depth right)
depth Empty = 0

layers1 = [
  Phantom (Point (-0.62) (-0.5)) (Point 0.62 0),
  -- Layer "0" Red [Line (Point (-0.52) 0) (Point 0.52 0)] "[line width=4pt]",
  Layer "1" Black tree1 "[line width=0.8pt]" ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict


