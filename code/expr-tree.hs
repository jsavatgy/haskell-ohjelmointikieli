
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr


tree = Mul (Add (Lit 3) (Lit 4)) (Lit 7)

eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y


