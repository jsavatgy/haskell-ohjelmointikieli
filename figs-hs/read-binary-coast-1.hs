import Eemian
import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString as ABS
import Data.Word
--import Data.Char (chr,isAlphaNum)
import Data.Attoparsec.Binary
--import DataBinaryIEEE754 

coastFile = "coastline/gshhs_c.b"
borderFile = "coastline/wdb_borders_c.b"
riverFile = "coastline/wdb_rivers_c.b"

word8ToInt :: Word8 -> Int
word8ToInt i = fromIntegral i

data ByteBlock = ByteBlock {
  sBytes :: [Word8] 
  } deriving Show

byteParser :: Parser ByteBlock
byteParser = do
  --bt1 <- count 4096 ABS.anyWord8
  bt1 <- count 256 ABS.anyWord8
  return ( 
    ByteBlock {
      sBytes = bt1 } )

blockSize = 16

lineaX d i = 
  Line (Point (x d i) k1) (Point (x d i) (k1 + ext i))
  where
    ext i
      | i == 0    = 24
      | otherwise = 8
    k1 = 0
    x d i = blockSize * (d*10+i)

textX d = p1 (show n) x 6 
  where
    x = blockSize * (intToDouble n)
    n = 10 * d
    p1 name x y = PlainTikzR 
      ("\\node[right] at (point1) {\\tiny " ++ 
      name ++ "};\n")
      [ReplacePt "point1" (Point (x-9) (y+10))]

lineaY d i = 
  Line (Point k1 (y d i)) (Point (k1 - ext i) (y d i))
  where
    ext i
      | i == 0    = 24
      | otherwise = 8
    k1 = 0
    y d i = -blockSize * (d*10+i)

textY d = p3 (show n) 6 y
  where
    y = -blockSize * (intToDouble n)
    n = 10 * d
    p3 name x y = PlainTikzR 
      ("\\node[left,rotate=90] at (point1) {\\tiny " ++ 
      name ++ "};\n")
      [ReplacePt "point1" (Point (x-22) (y+9))]


scaleX = 
  [lineaX d i | d <- [0..6], i <- [0..9], d*10+i<=64] ++
  [textX d | d <- [0..6]]

scaleY = 
  [lineaY d i | d <- [0..6], i <- [0..9], d*10+i<=64] ++
  [textY d | d <- [0..6]]


box (n,value) = FilledWith (colorlist !! valueI) (fromRect r1)
  where
    r1 = Rect (Point x1 y1)
              (Point (x1 + b) (y1 - b))
    y1 = -b * y
    x1 = b * x
    (x,y) = (intToDouble xI, intToDouble yI)
    (yI,xI) = n `divMod` 64
    valueI = word8ToInt value
    b = blockSize

boxes xs = map box (zip [0..] xs)

colorlist = [gray (n/255) | n <- [0..255]]


objects xs = scaleX ++ scaleY ++ boxes xs


layers1 xs = [
  Phantom (Point 0 0) (Point 896 896),
  Layer "S" Black (objects xs) "[line width=0.4pt]" ] 

tpict xs = tikzPicture (layers1 xs)



main :: IO ()
main = do
  file <- BS.readFile coastFile 
  case parseOnly byteParser file of
    Left error -> putStrLn $ "Error parsing: " ++ error
    Right x -> writeFile filename (tpict (sBytes x))
  where
    filename = "binary-coast.tikz"


