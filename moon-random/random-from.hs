import System.Random
import System.Directory
import System.Environment

trim = unwords . words

trimlines str =
  map trim (lines str) 

places x = filter (not . null) (trimlines x)
randomInt maxR = getStdRandom (randomR (0,maxR))

main = do
  args <- getArgs
  content <- readFile (head args)
  let 
    t = places content
    maxR = length t-1
  r <- randomInt maxR
  putStrLn (t!!r)

