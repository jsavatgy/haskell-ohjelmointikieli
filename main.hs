--f x = x * x
--g x y = x * x + y

f x = x * x
g x y = x * x + y

main = do
  putStrLn ("f 3 = " ++ show (f 3))
  putStrLn ("g 2 3 = " ++ show (g 2 3))

{-
x = 3
f = x
g x = x + 1
h = x + g x
k = x
  where
    x = 5
p x = x + x
  where
    x = 5

main = do
  putStrLn ("f 3 = " ++ show f)
  putStrLn ("g = " ++ show (g 1))
  putStrLn ("x = " ++ show x)
-}
