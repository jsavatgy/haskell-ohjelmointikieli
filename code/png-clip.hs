import System.IO.Unsafe
import System.Environment (getArgs)
import qualified Graphics.Rendering.Cairo as C
import System.FilePath.Posix (dropExtension)
import Numeric (readFloat)
import Eemian

checkArgs args = 
  case length args of
    5 -> (filename,x1,y1,x2,y2,newfile)
    _ -> error "parameters: inputfile x1 y1 x2 y2"
  where
    [a1,a2,a3,a4,a5] = args
    filename = a1
    [(x1,_)] = readFloat a2
    [(y1,_)] = readFloat a3
    [(x2,_)] = readFloat a4
    [(y2,_)] = readFloat a5
    newfile = dropExtension filename ++ "-clip.png"

main = do
  args <- getArgs
  let (filename,x1,y1,x2,y2,newfile) = checkArgs args
  surf <- return $ unsafeLoadPNG x1 y1 x2 y2 filename
  C.surfaceWriteToPNG surf newfile
   
imageSurfaceCreateFromPNG x1 y1 x2 y2 file =
  C.withImageSurfaceFromPNG file $ \png -> do
    surf <- C.createImageSurface C.FormatRGB24 
      (round (x2-x1)) (round (y2-y1))
    C.renderWith surf $ do
      C.setSourceSurface png (-x1) (-y1)
      C.paint
    return surf

unsafeLoadPNG x1 y1 x2 y2 file = unsafePerformIO $ 
  imageSurfaceCreateFromPNG x1 y1 x2 y2 file

-- png-clip Q412.png 3750.0 2250.0 4250.0 2750.0
