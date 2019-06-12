import System.IO.Unsafe
import System.Environment (getArgs)
import System.FilePath.Posix (dropExtension)
import System.Directory (doesFileExist)
import Numeric (readFloat)
import Eemian

checkArgs args = 
  case length args of
    5 -> (filename,x1,y1,x2,y2,newfile,pgwFile,newPgwFile)
    _ -> error "parameters: inputfile x1 y1 x2 y2"
  where
    [a1,a2,a3,a4,a5] = args
    filename = a1
    pgwFile = dropExtension filename ++ ".pgw"
    [(x1,_)] = readFloat a2
    [(y1,_)] = readFloat a3
    [(x2,_)] = readFloat a4
    [(y2,_)] = readFloat a5
    newfile = dropExtension filename ++ "-clip.png"
    newPgwFile = dropExtension filename ++ "-clip.pgw"

readFloat1 ('-':xs) = [(-x,r)]
  where
    [(x,r)] = readFloat xs
readFloat1 xs = readFloat xs

handlePgw x1 y1 x2 y2 content = ((e1,n1),(e2,n2))
  where
    (e1,n1) = (easting + (x1-0.5) * xScale,
      northing + (y1-0.5) * yScale)
    (e2,n2) = (easting + (x2-0.5) * xScale,
      northing + (y2-0.5) * yScale)
    [(xScale,_)] = readFloat1 a1
    [(ySkew,_)] = readFloat1 a2
    [(xSkew,_)] = readFloat1 a3
    [(yScale,_)] = readFloat1 a4
    [(easting,_)] = readFloat1 a5
    [(northing,_)] = readFloat1 a6
    [a1,a2,a3,a4,a5,a6] = lines content

main = do
  args <- getArgs
  let 
    (filename,x1,y1,x2,y2,newfile,pgwFile,newPgwFile) = 
      checkArgs args
  exists <- doesFileExist pgwFile
  if exists then do
    content <- readFile pgwFile
    let result = handlePgw x1 y1 x2 y2 content
    putStrLn (show result)
    return ()
  else do return ()

-- read-pgw Q412.png 3750.0 2250.0 4250.0 2750.0
