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

{-
> "aa\tbbb\tcccc" =~ "(.*)\t(.*)\t(.*)$" :: [[String]]
[["aa\tbbb\tcccc","aa","bbb","cccc"]]

"Oceanus Procellarum \tOcean of Storms \t18.4° N \t57.4° W \t2568" =~ "(.*)\t(.*)\t(.*)\t(.*)\t(.*)" :: [[String]]
splitOn "\t" "Sinus Concordiae\tSopusoinnun lahti\t10.8 N\t43.2 E\t142"
-}

finnished str = (a,b)
  where
    [a,b] = tb
    tb = splitOn "\t" str 

translate finnish1 str1 = [a,f,c,d,e]
  where
    [a,b,c,d,e] = str1
    result = lookup a finnish1
    f = case result of
      Just x -> x
      Nothing -> ""
  
backto xs = (intercalate "\t" xs1)
  where
    xs1 = [trim x | x <- xs]

tabulated str = [a,b,c,d,e] 
  where
    [a,b,c,d,e] = tb
    tb = splitOn "\t" str 

main = do
  content <- readFile "../moon-random/moon-orig2.txt"
  content2 <- readFile "../moon-random/kuu-suomeksi.txt"
  let 
    moon = filter (not . null) (lines content)
    finText = filter (not . null) (lines content2)
    coords1 = map tabulated moon
    finnish1 = map finnished finText
    coords2 = map (translate finnish1) coords1
    backto1 = map backto coords2
  mapM_ putStrLn backto1


