# Standardikirjasto `Prelude`

## Operaattoreita

```
($!) :: (a -> b) -> a -> b
(!!) :: [a] -> Int -> a
($) :: (a -> b) -> a -> b
(&&) :: Bool -> Bool -> Bool
(++) :: [a] -> [a] -> [a]
(.) :: (b -> c) -> (a -> b) -> a -> c
(=<<) :: Monad m => (a -> m b) -> m a -> m b
(^) :: (Num a, Integral b) => a -> b -> a
(^^) :: (Fractional a, Integral b) => a -> b -> a
(||) :: Bool -> Bool -> Bool
```

## Perustietotyypit

```
data Bool = False | True
type String = [Char]
data Either a b = Left a | Right b
data Maybe a = Nothing | Just a
```

## Funktioluettelo

```
all :: (a -> Bool) -> [a] -> Bool
and :: [Bool] -> Bool
any :: (a -> Bool) -> [a] -> Bool
appendFile :: FilePath -> String -> IO ()
asTypeOf :: a -> a -> a
break :: (a -> Bool) -> [a] -> ([a], [a])
catch :: IO a -> (IOError -> IO a) -> IO a
concat :: [[a]] -> [a]
concatMap :: (a -> [b]) -> [a] -> [b]
const :: a -> b -> a
curry :: ((a, b) -> c) -> a -> b -> c
cycle :: [a] -> [a]
drop :: Int -> [a] -> [a]
dropWhile :: (a -> Bool) -> [a] -> [a]
either :: (a -> c) -> (b -> c) -> Either a b -> c
elem :: Eq a => a -> [a] -> Bool
error :: [Char] -> a
even :: Integral a => a -> Bool
filter :: (a -> Bool) -> [a] -> [a]
flip :: (a -> b -> c) -> b -> a -> c
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl1 :: (a -> a -> a) -> [a] -> a
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr1 :: (a -> a -> a) -> [a] -> a
fromIntegral :: (Integral a, Num b) => a -> b
fst :: (a, b) -> a
gcd :: Integral a => a -> a -> a
getChar :: IO Char
getContents :: IO String
getLine :: IO String
head :: [a] -> a
id :: a -> a
init :: [a] -> [a]
interact :: (String -> String) -> IO ()
ioError :: IOError -> IO a
iterate :: (a -> a) -> a -> [a]
last :: [a] -> a
lcm :: Integral a => a -> a -> a
length :: [a] -> Int
lex :: ReadS String
lines :: String -> [String]
lookup :: Eq a => a -> [(a, b)] -> Maybe b
map :: (a -> b) -> [a] -> [b]
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
maximum :: Ord a => [a] -> a
maybe :: b -> (a -> b) -> Maybe a -> b
minimum :: Ord a => [a] -> a
not :: Bool -> Bool
notElem :: Eq a => a -> [a] -> Bool
null :: [a] -> Bool
odd :: Integral a => a -> Bool
or :: [Bool] -> Bool
otherwise :: Bool
print :: Show a => a -> IO ()
product :: Num a => [a] -> a
putChar :: Char -> IO ()
putStr :: String -> IO ()
putStrLn :: String -> IO ()
read :: Read a => String -> a
readFile :: FilePath -> IO String
readIO :: Read a => String -> IO a
readLn :: Read a => IO a
readParen :: Bool -> ReadS a -> ReadS a
reads :: Read a => ReadS a
realToFrac :: (Real a, Fractional b) => a -> b
repeat :: a -> [a]
replicate :: Int -> a -> [a]
reverse :: [a] -> [a]
scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl1 :: (a -> a -> a) -> [a] -> [a]
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr1 :: (a -> a -> a) -> [a] -> [a]
seq :: a -> b -> b
sequence :: Monad m => [m a] -> m [a]
sequence_ :: Monad m => [m a] -> m ()
showChar :: Char -> ShowS
showParen :: Bool -> ShowS -> ShowS
showString :: String -> ShowS
shows :: Show a => a -> ShowS
snd :: (a, b) -> b
span :: (a -> Bool) -> [a] -> ([a], [a])
splitAt :: Int -> [a] -> ([a], [a])
subtract :: Num a => a -> a -> a
sum :: Num a => [a] -> a
tail :: [a] -> [a]
take :: Int -> [a] -> [a]
takeWhile :: (a -> Bool) -> [a] -> [a]
uncurry :: (a -> b -> c) -> (a, b) -> c
undefined :: a
unlines :: [String] -> String
until :: (a -> Bool) -> (a -> a) -> a -> a
unwords :: [String] -> String
unzip :: [(a, b)] -> ([a], [b])
unzip3 :: [(a, b, c)] -> ([a], [b], [c])
userError :: String -> IOError
words :: String -> [String]
writeFile :: FilePath -> String -> IO ()
zip :: [a] -> [b] -> [(a, b)]
zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
```








## Luokka `Bounded`

```
class Bounded a where
  minBound :: a
  maxBound :: a
```




## Luokka `Floating`

```
class Fractional a => Floating a where
  pi :: a
  exp :: a -> a
  sqrt :: a -> a
  log :: a -> a
  (**) :: a -> a -> a
  logBase :: a -> a -> a
  sin :: a -> a
  tan :: a -> a
  cos :: a -> a
  asin :: a -> a
  atan :: a -> a
  acos :: a -> a
  sinh :: a -> a
  tanh :: a -> a
  cosh :: a -> a
  asinh :: a -> a
  atanh :: a -> a
  acosh :: a -> a
```

## Luokka `Fractional`

```
class Num a => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a
```

## Luokka `Integral`

```
class (Real a, Enum a) => Integral a where
  quot :: a -> a -> a
  rem :: a -> a -> a
  div :: a -> a -> a
  mod :: a -> a -> a
  quotRem :: a -> a -> (a, a)
  divMod :: a -> a -> (a, a)
  toInteger :: a -> Integer
```

## Luokka `Num`

```
class (Eq a, Show a) => Num a where
  (+) :: a -> a -> a
  (*) :: a -> a -> a
  (-) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
```

## Luokka `Ord`

```
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
```

## Luokka `RealFloat`

```
class (RealFrac a, Floating a) => RealFloat a where
  floatRadix :: a -> Integer
  floatDigits :: a -> Int
  floatRange :: a -> (Int, Int)
  decodeFloat :: a -> (Integer, Int)
  encodeFloat :: Integer -> Int -> a
  exponent :: a -> Int
  significand :: a -> a
  scaleFloat :: Int -> a -> a
  isNaN :: a -> Bool
  isInfinite :: a -> Bool
  isDenormalized :: a -> Bool
  isNegativeZero :: a -> Bool
  isIEEE :: a -> Bool
  atan2 :: a -> a -> a
```


## Luokka `RealFrac`

```
class (Real a, Fractional a) => RealFrac a where
  properFraction :: Integral b => a -> (b, a)
  truncate :: Integral b => a -> b
  round :: Integral b => a -> b
  ceiling :: Integral b => a -> b
  floor :: Integral b => a -> b
```



## Luokka `Monad`

```
class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  fail :: String -> m a
```


