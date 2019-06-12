import Data.List.Split
import Data.List
import Data.Char
import Eemian

trim = dropWhileEnd (not . isAlphaNum) .
  dropWhile (not . isAlphaNum)

{-
3100	600
3300	505
4100	125
-}

conv (a,b) = [a, show c]
  where
    c = round (3100 + 1000 * (600-d) / 475)
    d = intToDouble b
  
backto xs = (intercalate "\t" xs1)
  where
    xs1 = [trim x | x <- xs]

aged str = (a,read b :: Int)
  where
    [a,b] = tb
    tb = splitOn "\t" str 

main = do
  content <- readFile "../moon-random/age-of-surface-basalt.txt"
  let 
    moon = filter (not . null) (lines content)
    x1 = map aged moon
    x2 = map conv x1
  mapM_ putStrLn (map backto x2)


