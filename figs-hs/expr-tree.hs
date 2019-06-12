
import Eemian

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr
data Tree a = Empty | Leaf a | Branch (Tree a) (Tree a)

tree = Mul (Add (Lit 3) (Lit 4)) (Lit 7)

eval :: Expr -> Int
eval (Lit n) = n
eval (Add expr1 expr2)
  = eval expr1 + eval expr2
eval (Mul expr1 expr2)
  = eval expr1 * eval expr2

paint p (Lit n) = [Texttt p (show n) ""] 
paint p (Add expr1 expr2) = 
  paintBranch p "+" expr1 expr2
paint p (Mul expr1 expr2) = 
  paintBranch p "×" expr1 expr2


paintLeaf p x = [Texttt p (show x) ""]
paintBranch p str expr1 expr2 =
  [Texttt p str ""] ++
  [shorten r2 r2 p rightHand] ++ [shorten r2 r2 p leftHand] ++
  paint leftHand expr1 ++
  paint rightHand expr2
  where
    leftHand = towards (GON 250) (Point x y) r
    rightHand = towards (GON 350) (Point x y) r
    Point x y = p
    r = 1.5
    r2 = 0.2 * r

tree1 = paint (Point 0 0) tree

layers1 = [
  Phantom (Point (-10) 0) (Point 10 (-4)),
  Layer "1" Black tree1 "[line width=0.8pt]" ]

tpict = tikzPicture layers1

main = do
  putStrLn tpict

