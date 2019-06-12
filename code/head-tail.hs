
head (x:_)  = x
tail (_:xs) = xs

f (x:xs) = show x ++ show xs
