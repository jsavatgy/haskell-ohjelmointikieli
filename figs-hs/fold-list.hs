
import Eemian

data Direction = L | R
  deriving (Eq)

folded3 p dir zero f [] = [Texttt p zero ""] 
folded3 p dir zero f (z:zs) =
  [shorten r2 r2 p rightHand] ++ [shorten r2 r2 p leftHand] ++
  [Texttt p f ""] ++
  [Texttt lr1 (show z) ""] ++
  folded3 lr2 dir zero f zs
  where
    lr1
      | dir == L = rightHand
      | otherwise = leftHand
    lr2
      | dir == L = leftHand
      | otherwise = rightHand
    rightHand = towards (GON 350) (Point x y) r
    leftHand = towards (GON 250) (Point x y) r
    Point x y = p
    r = 1.5
    r2 = 0.2 * r

--folded1 = folded3 (Point (-2) 0) L 0 "+" [1..4]
folded2 = folded3 (Point 2 0) R "[]" ":" [1..4]

layers1 = [
  Phantom (Point (-10) 0) (Point 10 (-4)),
  --Layer "1" Black folded1 "[line width=0.8pt]",
  Layer "2" Black folded2 "[line width=0.8pt]" ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict

