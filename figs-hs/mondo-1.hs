import Data.List.Split
import Data.List
import Data.Maybe
import Data.Char
import Eemian

coastFile = "coastline/gshhs_c.txt"

simplify (x:y:zs)
  | dist x y < 1.2 = simplify (x:zs)
  | otherwise    = x : y : simplify zs
simplify xs = xs

untab str = splitOn "\t" str

untabPts str = Point x2 y
  where
    x2 = if x > 180 then 180 else x
    [x,y] = map readd [a,b]
    [a,b] = untab str
    readd x = read x :: Double

pgPts xs = map untabPts xs

isCommentEtc str =
  "#" `isPrefixOf` str || ">" `isPrefixOf` str

s = 400

layers1 pgs = [
  Phantom (Point (-s) (-s)) (Point s s),
  Layer "1" Black pgs "[line width=0.5pt,line join=round,line cap=round]",
  Empty
  ]

tpict c = tikzPicture (layers1 c)

main = do
  content <- readFile coastFile
  let 
    x1 = splitWhen isCommentEtc (lines content)
    x2 = filter (not . null) x1
    x3 = map pgPts x2
    x4 = map simplify x3
    x5 = map PolyLine x4
  putStrLn (tpict x5)

