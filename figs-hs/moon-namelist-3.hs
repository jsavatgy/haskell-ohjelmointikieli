import Data.List.Split
import Data.List
import Data.Char
import Eemian

trim = dropWhileEnd (not . isAlphaNum) .
  dropWhile (not . isAlphaNum)

-- trim2 = dropWhileEnd (not . isAlphaNum) . dropWhile (not . isAlphaNum)

coordList2 coords1 = 
  "\\bgroup" ++
  "\\def\\arraystretch{0.7}" ++
  "\\begin{tabular}{ l l r r r}" ++
  concat [
    latinName ++ " &" ++ 
    englishName ++ "&" ++ 
    latitude ++ "&" ++ 
    longitude ++ "&" ++ 
    radius ++ "\\\\" 
    | [latinName,englishName,latitude,longitude,radius] 
      <- coords1] ++
  "\\end{tabular}" ++
  "\\egroup"

coordList1 c = [Texttt pt1 (t (coordList2 c))
  "below right"]
  where
    pt1 = Point 0 0
    t str = "{"  ++ textSizeLatex (-2) ++ str ++ "}"

layers1 c = [
  Phantom (Point 0 0) (Point 10 20),
  Layer "1" Black (coordList1 c) "[line width=0.8pt]" ]

tpict c = tikzPicture (layers1 c)


aged str = (a,b)
  where
    [a,b] = tb
    tb = splitOn "\t" str 

lookup1 table2 table1 = [a,c1,d1,e,f]
  where
    [c1,d1] = map brt [c,d]
    [a,b,c,d,e] = table1
    f = case lookup a table2 of
      Just x -> x
      Nothing -> ""
    brt s = addMinus s ++ takeWhile (/= ' ') s
    addMinus s = if last s `elem` "SW" then "-" else ""

backto xs = (intercalate "\t" xs1)
  where
    xs1 = xs

tabulated str = map trim (splitOn "\t" str)

main = do
  content <- readFile "../moon-random/moon-list.txt"
  content2 <- readFile "../moon-random/surface-basalt-age.txt"
  let 
    moon = filter (not . null) (lines content)
    ageText = filter (not . null) (lines content2)
    coords1 = map tabulated moon
    aged1 = map aged ageText
    coords2 = map (lookup1 aged1) coords1
    backto1 = map backto coords2
  mapM_ putStrLn backto1
  --putStrLn (show aged1)
  --putStrLn (show coords1)


