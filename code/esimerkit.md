# Kirjan komentoriviesimerkit

## Standardikirjaston funktioita

```haskell
ghci

64*16

min 2 3
max 2 3

2 `min` 3
2 `max` 3

(+) 2 3

odd 2
even 2
odd 3
even 3

(+) 2 3
(-) 3 4
(*) 4 5
(/) 1 2
(-1) + (-2)
2 + 3
3 - 4
4 * 5
1 / 2

7 `div` 3
7 `mod` 3
7 `divMod` 3
abs (-3)

not True
not False
True && False
True || False
and [True,False,True]
or [True,False,True]

a = 2
b = 3
c = a + b
c

p = True
r = False
s = p || r
s

c1 = 'e'
c2 = 'o'
q = c1 < c2
q

plus = (+)
a `plus` b
(×) = (*)
3 × 4

s = "Mare Australe"
size = length
size s


ts = [2,3,5,7,11]
ts

ts = [2,3,5,7,11]
head ts
tail ts
null ts
null []

ts = [2,3,5,7,11]
any (even) ts
all (odd) ts
any (>10) ts
all (<10) ts

ts = [2,3,5,7,11]
sum ts
product ts
minimum ts
maximum ts
last ts
init ts
take 3 ts
drop 2 ts
reverse ts

ts = [2,3,5,7,11]
length ts
size = length
size ts
(×) = (*)
3 × 4

ts = [2,3,5,7,11]
splitAt 2 ts
map (*2) ts
map (+11) ts
map negate ts
filter (<10) ts
filter even ts
filter odd ts

p = (2,3)
fst p
snd p

:type fst
:type snd

(,) 1 2
(,,) 3 4 7

(1,'a')
(1,2,'b')
((1,'a'),(1,2,'b'),1)

[1..6] 
[3..7]
[6,5..1]
[1,3..11]
[2,4..10]
[0,4..24]

[1.5..5.5]
['a'..'g']

filter (<10) [1..]
repeat 1
repeat [1,2]
cycle [1,2]

take 5 [1..]
head [1..]
takeWhile (<10) [1,3..]
take 10 (repeat 1)
take 5 (repeat [1,2])
take 10 (cycle [1,2])

ts = [2,3,5,7,11]
zip ts [1..]
zip ts ts
zip ts (tail ts)
zip [1..] ts

:help
:browse Prelude
:info max
:info Int
3*4
it+1
:! date

:{
s = a + b
 where
  a = 5
  b = 7
:}
s

:set prompt  "\ESC[34m\STX> \ESC[m\STX"
:set prompt2 "\ESC[34m\STX| \ESC[m\STX"
:quit


:{
s = a + b
  where
    a = 5
    b = 7
:}

s = a + b where { a = 5; b = 7 }

```

## Listat ja rekursio

```haskell
xs = [3,5,7,11]
head xs
tail xs
head (tail xs)
tail (tail xs)

3:(5:(7:(11:[])))
3:5:7:11:[]
:info (:)

:type (:)
3 : [5,7]
5 : [11]

:type (++)
[2,3] ++ [5,7,11]
[5] ++ [11]

['M','e','d','i','i'] 
'S':'m':'y':"thii"
"Imbr" ++ "ium"

"" ++ "Nubium"
"Australe" ++ ""
"S" ++ "mythii"
"Angui" ++ "s"

'M' ++ "ortis"
'O':'d':'i':'i'
'A':"utumni"
'S':'o':'m':'n':'i':[]
'S':'p':'e':"i"

(:) 1 []
(:) 2 [3,4]
(:) 'G' "audii"
(++) [3,4] [5,6]
(++) "Fecun" "ditatis"

x:xs = [1,2,3,4]
x
xs

[(x,y) | x <- [1..5], y <- [12..16], odd x, even y]

[x+10 | x <- [1,2,3]]
[x*x | x <- [1..5], odd x]
[(i,j) | i <- [1..2], j <- [1..2]]
[(i,j) | i <- [1..3], j <- [i..3]]
[(i,j) | i <- [1..3], j <- [1..3], i < j]
[(i,j) | i <- [1..4], even i, j <- [i+1..4], odd j]

:{

  qsort [] = []
  qsort (x:xs) =  qsort [y | y <- xs, y<x]
               ++ [x]
               ++ qsort [y | y <- xs, y>=x]
:}

qsort [9,2,6,11,3,7,4,5]
qsort "Tranquillitatis"
```

## Funktioiden määrittely

```haskell
f x = x * x
f 3
g x y = x * x + y
g 2 3

:{
f = a + b
  where
    a = c + 2
    b = c + 4
    c = 3
:}


:{
f = let
    a = c + 2
    b = c + 4
    c = 3
  in a + b
:}


:{
x = 3
f = x
g x = x + 1
h = x + g x
i = g 1
j = p 2
k = let
    x = 5
    g = 10
  in x + g
p x = x + x
  where
    x = 5
:}

x
f
g 1
h
k
p 2

(f,h,i,j,k,x)
(x,f,g 1,h,k,p 2)


i = 2
i
s = "Serenitatis"
s
ls = [1,2,3,4]
ls

f x = x * x
g x y = x * x + y
putStrLn ("f 3 = " ++ show (f 3))
putStrLn ("g 2 3 = " ++ show (g 2 3))

g x y = x * x + y
s = "g 2 3 = " ++ show (g 2 3)
s
putStrLn s
show 7
show g


(g 2 3)
print (g 2 3)
print "Luxuriae"
putStrLn "Luxuriae"
:t print
:t putStrLn


:{ 
f x = x * x
g x y = x * x + y

main = do
  putStrLn ("f 3 = " ++ show (f 3))
  putStrLn ("g 2 3 = " ++ show (g 2 3))
:}
main

:{
f 1 = "one"
f 2 = "two"
f 3 = "three"
f x = "other: " ++ show x
:}
map f [1..6]

data Bool = False | True
:{
show True  = "True"
show False = "False"
:}
map show [True,False]

f ((:) x y) = "head = " ++ show x ++ ", tail = " ++ show y
putStrLn (f "Marginis")

f (x:y) =  "head = " ++ show x ++ ", tail = " ++ show y
putStrLn (f "Humorum")

ts = [2,3,5,7,11]
p = splitAt 2 ts
p
(a,b) = p
a
b
(,) a b = splitAt 2 ts

:{
planet name d
  | d < 3.0   = name ++ ": small planet"
  | otherwise = name ++ ": giant planet"
:}
planet "Earth" 1.0
planet "Saturn" 9.4

(:) 'H' "umorum"

:type (:)


data Shape = Circle Float | Rectangle Float Float
:{
area (Circle r) = pi * r * r
area (Rectangle a b) = a * b
:}
c = Circle 10
d = Rectangle 2 5
area c
area d


:{
let
  pairs (x:y:zs) = (x,y) : pairs (y:zs)
  pairs (y:zs) = []
:}
print (pairs [1..5])

:{
pairs' (y:zs) = []
pairs' (x:y:zs) = (x,y) : pairs' (y:zs)
:}
print (pairs' [1..5])

:{
length [] = 0
length (x:xs) = 1 + length xs

sum [] = 0
sum (x:xs) = x + sum xs

product [] = 1
product (x:xs) = x * product xs

factorial 0 = 1
factorial n = n * factorial (n-1)

[] ++ ys     = ys
(x:xs) ++ ys = x : xs ++ ys
:}
length [1..100]
sum [1..100]
product [1..10]
factorial 10
[1..3] ++ [4..6]

:{
let
  ones = 1:ones
  numsFrom n = n:numsFrom(n+1)
  squares = map (^2) (numsFrom 0)
:}
take 5 ones
take 5 (numsFrom 5)
take 5 squares

```

## Tyypillisiä funktiorakenteita

```haskell

f ∘ g = \x -> f (g x)
:t (∘)
f = (* 4)
g = (+ 2)
(f ∘ g) 5

(∘) = (.)
f = (* 4)
g = (+ 2)
(f ∘ g) 5


:{
let
  allEqual []     = True
  allEqual (x:xs) = all (== x) xs
:}

foldr (+) 0 [1,2,3]
foldl (+) 0 [1,2,3]
foldr (-) 0 [1,2,3]
foldl (-) 0 [1,2,3]

foldr (+) 0 [1, 2, 3]
1 + foldr (+) 0 [2, 3]
1 + (2 + foldr (+) 0 [3])
1 + (2 + (3 + foldr (+) 0 []))
1 + (2 + (3 + 0))
6

foldl (+) 0 [1, 2, 3]
foldl (+) ((+) 0 1) [2, 3]
foldl (+) ((+) ((+) 0 1) 2) [3]
foldl (+) ((+) ((+) ((+) 0 1) 2) 3) []
(+) ((+) ((+) 0 1) 2) 3
6

foldr (-) 0 [1, 2, 3]
1 - foldr (-) 0 [2, 3]
1 - (2 - foldr (-) 0 [3])
1 - (2 - (3 - foldr (-) 0 []))
1 - (2 - (3 - 0))
2

foldl (-) 0 [1, 2, 3]
foldl (-) ((-) 0 1) [2, 3]
foldl (-) ((-) ((-) 0 1) 2) [3]
foldl (-) ((-) ((-) ((-) 0 1) 2) 3) []
(-) ((-) ((-) 0 1) 2) 3
-6

:{
let
  f :: Int -> Int -> Int
  f x y = x + y

  g :: Int -> (Int -> Int)
  (g x) y = x + y

  plusOne :: Int -> Int 
  plusOne = f 1
:}

:t f
:t f 1
:t f 1 2


plusOne = f 1
plusOne 2
plusOne 3

filter (\x -> 3 <= x && x <= 5) [1..9] 
filter (\x -> snd x `mod` 2 == 1 && fst x <= 'e') (zip ['a'..'z'] [1..])
map (\x -> x `max` 0) [-1..2]
map (\x -> x `mod` 2) [1..4]
map (\x -> (x,x))  [1..3]

map (2 +) [1,2,3] 
map (3 *) [1,2,3] 
map (/ 2) [2,4,8] 

t = zip [1..5] ['a'..]
t
['a']

g (a,b) = show a ++ " " ++ [b]
g (1,'a')
map g t

f a b = show a ++ " " ++ [b]
f 1 'a'
map (\(a,b) -> f a b) t

map (uncurry f) t
:t curry
:t uncurry
zipWith f [1..5] ['a'..]

:info (+)

2 + 3 * 4 == 2 + (3 * 4)
3 - 2 - 1 == (3 - 2) - 1
2 ^ 2 ^ 3 == 2 ^ (2 ^ 3)

mmap f = foldr ((:).f) []
mmap (2*) [1..5]

ffilter p = foldr (\x xs -> if p x then x : xs else xs) []
ffilter odd [1..10]

foldr (:) [] [4,3,2,1]
foldr (:) [1] [4,3,2] 
foldr (:) [2,1] [4,3] 
foldr (*) 1 [1,2,3,4] 
foldr (+) 0 [1,2,3,4] 

take 3 (reverse (filter even [1..10]))
f = take 3 . reverse . filter even
f [1..10]

:{
let
  f = take 3
  g = reverse
  h = filter even
  k = f . g . h
:}
f [1..10]
g [1..10]
h [1..10]
take 3 (reverse (filter even [1..10]))
(take 3 . reverse . filter even) [1..10]
(f . g . h) [1..10]
k [1..10]
k $ [1..10]
f . g . h $ [1..10]
take 3 . reverse . filter even $ [1..10]

(filter even $) [1..10]
filter even $ [1..10]
filter even ($ [1..10])

f $ x = f x; infixr 0 $
(filter even $) [1..10]
filter even $ [1..10]
filter even ($ [1..10])

f $ x = f x; infixr 9 $
(filter even $) [1..10]
filter even $ [1..10]
filter even ($ [1..10])

f $ x = f x; infixr 10 $
(filter even $) [1..10]
filter even $ [1..10]
filter even ($ [1..10])

infixr 0 $; f $ x = f x
g . h $ [1..10]
infixr 9 $; f $ x = f x
g . h $ [1..10]
infixl 0 $; f $ x = f x
g . h $ [1..10]
infixl 9 $; f $ x = f x
g . h $ [1..10]
m = g . h
g (h $ [1..10])
m $ [1..10]

(++ "um") "Vapor"
($ 3) (4 +) 
map (++ "um") ["Vapor","Undar","Humor"]
map ($ 3) [(4 +),(3 *),(2 ^)]
```

## Tietotyypit

```haskell
f n = 2 ^ n
map f [0..8]

:{
toBinary 0 = "0"
toBinary 1 = "1"
toBinary r = toBinary (r `div` 2) ++
  if (r `mod` 2 == 1) then "1" else "0"
:}

map toBinary [0..15]

t = map (sequence . flip replicate "01") [0..2]
t
concat t

:t False
:t True
not False

data Bool = False | True

:{

  toBinary 0 = [ 0 ]
  toBinary n = toBinary ( n `quot` 2 ) ++ [ n `rem` 2 ]
:}
toBinary 255

import Text.Printf
inBinary i = printf "The value of %d in binary is: %08b\n" i i
inBinary 5

import Data.Char
ord 'A'
ord 'a'
inBinary (ord 'A')
inBinary (ord 'a')

(['A'..'Z'],['a'..'z'])
isAscii 'a'
isAscii 'α'

import Data.Char
ord 'æ'
isAscii 'æ'
isLatin1 'æ'

minBound :: Bool
maxBound :: Bool
minBound :: Char
maxBound :: Char

import Data.Word
minBound :: (Word8,Word16,Word32,Word64)
maxBound :: (Word8,Word16,Word32,Word64)
maxBound :: Word

import Data.Int
minBound :: (Int8,Int16,Int32,Int64)
maxBound :: (Int8,Int16,Int32,Int64)
minBound :: Int
maxBound :: Int

m = 9223372036854775807
zipWith ($) [(+0),(+1),(+2)] (repeat (m :: Integer))
zipWith ($) [(+0),(+1),(+2)] (repeat (m :: Int))

tau = 6.28318530717958647692
tau :: Float
tau :: Double

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
:t Monday
data Piece =  King | Queen | Bishop | Knight | Rook | Pawn
:t Queen
data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
:t King

data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
data Suit = Club | Diamond | Heart | Spade
data Card = Card Suit CardValue
:t Card
:t Card Heart
:t Card Heart Eight
:t Card Spade Ten
:t Card Diamond Seven


data List a = Nil | Cons a (List a)

data List a = Nil | Cons a (List a) deriving Show
:t Nil
:t Cons 2 Nil
:t Cons 3 (Cons 2 Nil)
:t Cons 4 (Cons 3 (Cons 2 Nil))

Cons 'a' (Cons 'b' (Cons 'c' Nil))
Cons "A." (Cons "B." (Cons "C." Nil))
Cons True (Cons False (Cons True Nil))

:t Cons 'a' (Cons 'b' Nil)
:t Cons "A." (Cons "B." Nil)
:t Cons True (Cons False Nil)


data Maybe a = Nothing | Just a  deriving Show
b = 2 :: Int
Just b
:t Just b
lookup 'c' [('a',0),('b',1),('c',2)]
lookup 'd' [('a',0),('b',1),('c',2)]
:t lookup

data Either a b = Left a | Right b  deriving Show

:{
upper x
  | x `elem` ['A'..'Z']  =  Right x
  | otherwise  =  Left x
isIt (Right c) = "Yes: " ++ [c] 
isIt (Left c)  = "No: "  ++ [c] 
:}
upper 'T'
upper 't'
isIt (upper 'T')
isIt (upper 't')
```

## Tyyppiluokat

```haskell
data Bool = False | True
True == True
Prelude.True == Prelude.True
data Bool = False | True deriving (Eq)
True == True
True
data Bool = False | True deriving (Eq,Show)
True
show True
show False

:{
data Boolean = F | T

instance Eq Boolean where 
  T == T  =  True
  F == F  =  True
  _ == _  =  False
:}
T == F
T /= F


:{
data ABC = A | B | C  deriving (Eq,Show)
instance Ord ABC where 
  A <= B  =  True
  B <= C  =  True
  A <= C  =  True
  _ <= _  =  False
:}

:{
data Roshambo = Rock | Paper | Scissors  deriving (Eq,Show)
instance Ord Roshambo where 
  Paper <= Scissors  =  True
  Scissors <= Rock  =  True
  Rock <= Paper  =  True
  _ <= _  =  False
:}

Scissors > Paper
Paper > Rock
Rock > Scissors
max Paper Scissors
max Rock Paper

:{
data Color = Black | White | Green | Blue | Violet | 
  Red | Orange | Yellow | Pink | Brown
  deriving (Enum,Show)
:}

[Green .. Brown]

fromEnum Blue
enumFrom Yellow
enumFromThen Violet Orange
enumFromThenTo 1 3 11
pred White
succ White
succ 4

:{
data Color = Black | White | Green | Blue | Violet | 
  Red | Orange | Yellow | Pink | Brown
  deriving (Bounded,Show)
:}

minBound :: Color
maxBound :: Color

:{
data Color = Black | White | Green | Blue | Violet | 
  Red | Orange | Yellow | Pink | Brown
  deriving (Read,Show)
:}

read "Pink" :: Color
read "5" :: Int




e1 = print "e1"
e2 = print "e2"
do e1; e2
e1 >> e2


:{
do
  e1
  e2
:}
e1 >> e2

do 
  p <- getLine; 
  print p

getLine >>= (\p -> print p)

add mx my = do
    x <- mx
    y <- my
    return (x + y)

add mx my =
    case mx of
        Nothing -> Nothing
        Just x  -> case my of
                       Nothing -> Nothing
                       Just y  -> Just (x + y)

fail1 = do (x:xs) <- Just []; return x 
just1 = do (x:xs) <- Just [1]; return x 
just2 = do (x:xs) <- Just [2,1]; return x 

:{
sndGrd x = do
  (x:xs) <- Just x
  (x:xs) <- Just x
  return x
:}

ts = [1,2,3,4]
ts >>= \x -> [x,-x] 
xs = map (\x -> [x,-x]) ts
xs
concat xs


concatMap (\_ -> [1]) [3, 2, 1]
[3,2,1] >> [1]
do [1,2,3]; return 1

```





## Kuviot

```haskell

:{
data Point = Point Double Double
  deriving Show
:}

p1 = Point 1 1
p2 = Point 3 2

p1
p2
:type [p1,p2]

:{
data Shape = Circle Double Point
  | Line Point Point
  | PolyLine [Point]
  | Arc Double Point Angle Angle
  deriving Show
:}

line1 = Line (Point 4 1) (Point 3 3)
circle1 = Circle 0.8 (Point 2 4)
pts = [(1,1),(1,2),(2,2),(2,1),(3,1)]
polyline1 = PolyLine [Point x y | (x,y) <- pts]

line1
circle1
polyline1

shapes1 = [line1,circle1,polyline1]
shapes1
:t shapes1

p1 = Point 1 1
p2 = Point 3 2
c1 = Circle 2.5 p1
c2 = Circle 2.5 p2

circleCircleIntersections c1 c2


p1 = Point 1 1
p2 = Point 3 2
c1 = Circle 2.5 p1
c2 = Circle 2.5 p2
[p3,p4] = circleCircleIntersections c1 c2
v0 = eastVector p1
v1 = mkVector p1 p3
v2 = mkVector p1 p4
t1 = angleBt v1 v0
t2 = angleBt v2 v0

v3 = mkVector p2 p3
v4 = mkVector p2 p4
t3 = angleBt v0 v3
t4 = angleBt v0 v4

:l Shapes.hs
import Shapes
:{
pts = [pointOfEllipse 3 2 t | t <- [0,dt..twopi]]
  where
    dt = twopi / n
    n = 120
:}

pl1 = PolyLine pts

lengthPL pl1

:{
dotsEllipse = pts
  where
    pts = [alongPL pl1 (s * l2) | s <- [1..n]]
    l2 = l1 / n
    l1 = lengthPL pl1
    n = 15
:}

:t dotsEllipse

sin1 (DEG 30)
sin1 (RAD (halfpi/3))
```

## Teksti, sanat ja kirjaimet

```haskell
content <- readFile "moon1.txt"
content
lines content
words content

content <- readFile "kolme-muskettisoturia.txt"
t = [(toUpper c, 1) | c <- content, isLetter c]
length t
take 10 t


```


