# Tyyppiluokat

Kun aiemmin määrittelimme tietotyypin `Bool`, ohitimme määrittelyn avulla tietotyypin määrittelyn standardikirjastossa `Prelude`. Määrittelemämme tietotyyppi ei täysin vastaa standardikirjaston määritelmää. Esimerkiksi oman tietotyyppimme vertailuoperaatio `(==)` johtaa virheilmoitukseen tietotyypille tunnistamattomasta operaatiosta. 

```haskell
> data Bool = False | True
> True == True
"ERROR: No instance for (Eq Bool) arising \
       \from a use of ‘==’."
```

Sen sijaan standardikirjaston tietotyypille vertailu onnistuu.

```haskell
> Prelude.True == Prelude.True
True
```

Havaitsemme omalta tietotyypiltämme puuttuvan muitakin Haskell-kielen perustyypeille tyypillisiä ominaisuuksia. Tilanne on onneksi helppo korjata. Voimme antaa tietotyyppimme *periä* ominaisuuksia tyyppiluokilta. Yhtäsuuruusoperaatio on määritelty tyyppiluokassa `Eq`. Mikäli operaation oletustoteutus vastaa tarkoituksiamme, tyyppiluokan periminen on yksinkertainen toimenpide. Se tapahtuu tietotyypin määrittelyn yhteydessä avainsanalla `deriving`. 

Automaattisen periytymisen seurauksena tyypistä `Bool` tulee tyyppiluokan `Eq` jäsen, eli sille on määritelty tyyppiluokan ilmentymä eli *instanssi*. Funktio `(==)` tulee tällöin *ylikuormitetuksi* periaatteella: alkio on yhtäsuuri itsensä kanssa ja erisuuri muiden alkioiden suhteen.

```haskell
> data Bool = False | True  deriving (Eq)
> True == True
True
```

Edellä kuvattu ilmiö toistuu, kun yritämme tulostaa arvon `True`.

```haskell
> True
"ERROR: No instance for (Show Bool) arising \
       \from a use of ‘print’."
```

Voimme korjata tilanteen antamalla tietotyyppimme periä myös tyyppiluokan `Show`. Tietotyypistä `Bool` tulee näin ollen tyyppiluokan `Show` jäsen. Funktion `show` ylikuormitetuksi toteutukseksi tulee alkion nimi merkkijonoksi muutettuna.

```haskell
> data Bool = False | True deriving (Eq,Show)
> True
True
> show True
"True"
> show False
"False"
```

Tyypillisimmin käyttämämme automaattisesti periytyvien tyyppiluokkien luettelo on `(Eq,Ord,Show,Read)`. Kertaamme näistä tutut ja esittelemme uudet.

## Tyyppiluokka `Eq`

Standardikirjasto `Prelude` määrittelee tyyppiluokalle `Eq` yhtäsuuruus- ja erisuuruusoperaatiot (`==` ja `/=`). Näistä riittää toisen määrittely, toinen määritelmä seuraa ensimmäisen negaationa.

```haskell
class  Eq a  where
  (==), (/=) :: a -> a -> Bool
  x /= y     =  not (x == y)
  x == y     =  not (x /= y)
```

Määrittelemme seuraavassa tyypin `Boolean`, joka voi saada toisen arvoista `T` tai `F`. Kirjoitamme tyypille myös tyyppiluokan `Eq` instanssin.

```haskell
data Boolean = F | T

instance Eq Boolean where 
  T == T  =  True
  F == F  =  True
  _ == _  =  False
```

Saamme

```haskell
> T == F
False
> T == T
True
> T /= F
True
```

## Tyyppiluokka `Ord`

Tyyppiluokka `Ord` määrittää matemaattiset vertailuoperaatiot `<`, `<=`, `>` ja `>=`. Samoin se määrittää funktiot `min`, `max` ja `compare`.

Luokan pienin vaadittava määrittely on joko operaatio `<=` tai funktio `compare`. Luokan jäsenen tulee kuulua myös luokkaan `Eq`.

```haskell
  class  (Eq a) => Ord a  where  
    compare              :: a -> a -> Ordering  
    (<), (<=), (>=), (>) :: a -> a -> Bool  
    max, min             :: a -> a -> a  
 
    compare x y | x == y    = EQ  
                | x <= y    = LT  
                | otherwise = GT  
 
    x <= y  = compare x y /= GT  
    x <  y  = compare x y == LT  
    x >= y  = compare x y /= LT  
    x >  y  = compare x y == GT  
 
    -- Note that (min x y, max x y) = (x,y) or (y,x)  
    max x y | x <= y    =  y  
            | otherwise =  x  
    min x y | x <= y    =  x  
            | otherwise =  y 
```

Tyyppiluokan `Ord` avulla voimme kirjoittaa kivi-paperi-sakset -leikin toteutuksen. Leikin säännöt ovat yksinkertaiset: sakset voittaa paperin, kivi voittaa sakset ja paperi voittaa kiven. Huomaamme, että tyyppiluokassa `Ord` alkiot voivat muodostaa järjestysominaisuuden suhteen silmukan, eli kahdesta alkiosta voimme valita suuremman (`max`), mutta suurinta (`maximum`) ja pienintä (`minimum`) alkiota ei ole.

```haskell
data Roshambo = Rock | Paper | Scissors
  deriving (Eq,Show)
instance Ord Roshambo where 
  Paper <= Scissors  =  True
  Scissors <= Rock  =  True
  Rock <= Paper  =  True
  _ <= _  =  False
```

Nyt saamme

```
> Scissors > Paper
True
> Paper > Rock
True
> Rock > Scissors
True
> max Paper Scissors
Scissors
> max Rock Paper
Paper
```

## Tyyppiluokka `Enum`

Tyyppiluokka `Enum` tekee tyypin arvoista lueteltavia (*enumerable*).

Voimme luetella esimerkiksi haluamamme värit haluamassamme järjestyksessä.

```haskell
data Color = Black | White | Green | Blue | Violet | 
  Red | Orange | Yellow | Pink | Brown
  deriving (Enum,Show)
```

Nyt tyyppiluokan `Enum` periminen mahdollistaa arvoilla rajatun luettelon tulostamisen.

```haskell
> [Green .. Brown]
[Green,Blue,Violet,Red,Orange,Yellow,Pink,Brown]
```

Tyyppiluokka `Enum` määrittelee funktiot `succ`, `pred`, `fromEnum`, `enumFrom`, `enumFromThen`, `enumFromTo` ja `enumFromThenTo`. Näistä `succ` palauttaa edellä olevan arvon ja `pred` jäljessä tulevan. Funktiot `enumFrom`, `enumFromThen`, `enumFromTo` ja `enumFromThenTo` määrittelevät Haskell-kielen lyhennysmerkinnän `[n,m..t]` muuttujat `n` (*from*), `m` (*then*) ja `t` (*to*). 

Funktio `fromEnum` palauttaa alkion järjestysluvun.

```haskell
> fromEnum Blue
3
> enumFrom Yellow
[Yellow,Pink,Brown]
> enumFromThen Violet Orange
[Violet,Orange,Pink]
> enumFromThenTo 1 3 11
[1,3,5,7,9,11]
> pred White
Black
> succ White
Green
> succ 4
5
```

## Tyyppiluokka `Bounded`

Tyyppiluokka `Bounded` määrittelee funktiot `minBound` ja `maxBound`.

```haskell
data Color = Black | White | Green | Blue | Violet | 
  Red | Orange | Yellow | Pink | Brown
  deriving (Bounded,Show)
```

Funktio `minBound` palauttaa tyypin alarajan ja funktio `maxBound` ylärajan.

```haskell
> minBound :: Color
Black
> maxBound :: Color
Brown
```

## Tyyppiluokka `Show`

Tyyppiluokka `Show` määrittelee muun muassa funktion `show`. Funktion tehtävä on tekstuaalisen esityksen tuottaminen tyypin arvoille.

```haskell
> show Green
"Green"
> show 12
"12"
```

## Tyyppiluokka `Read`

Tyyppiluokka `Read` mahdollistaa tekstuaalisen esityksen lukemisen tiettyä tyyppiä olevaksi arvoksi.

```haskell
data Color = Black | White | Green | Blue | Violet | 
  Red | Orange | Yellow | Pink | Brown
  deriving (Read,Show)
```

Esimerkiksi merkkijonosta `"Pink"` saamme tyypin `Color` arvon `Pink` ja merkkijonosta `"5"` tyypin `Int` arvon 5.

```haskell
> read "Pink" :: Color
Pink
> read "5" :: Int
5
```

## Tyyppiluokka `Functor`

Tyyppiluokka `Functor` määrittelee kuvausfunktion `fmap`.

Haskell-kielen listat `[a]` kuuluvat tyyppiluokkaan `Functor`. Niiden tyyppiluokan `Functor` instanssi määrittelee kuvausfunktion `map` funktion `fmap` toteutuksena. 

```haskell
instance Functor [] where  
  fmap = map  
```

Käytimme aiemmin funktiota `map` numeerisen listan alkioiden laskutoimituksiin.

```haskell
> map (^ 2) [1..7]
[1,4,9,16,25,36,49]
```

Funktion `map` tyyppiallekirjoitus on `(a -> b) -> [a] -> [b]`
eli prefix-muodossa `(a -> b) -> [] a -> [] b`. Tämä on sama kuin funktion `fmap` tyyppiallekirjoitus `(a -> b) -> f a -> f b`, kun `f` = `[]`.

```haskell
> :t map
map :: (a -> b) -> [a] -> [b]
> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
```

Tyyppiluokan `Functor` merkitys käy selville esimerkiksi tyypin `Maybe` instanssin määrittelystä.

```haskell
instance Functor Maybe where
  fmap f Nothing  = Nothing
  fmap f (Just x) = Just (f x)
```

Tässä olennainen asia on, että funktio `fmap` ei käytä funktiota `f` arvolle `Nothing`. Tämä on mielekästä, sillä arvon `Nothing` merkitys usein on ilmaista funktiokutsun epäonnistumisesta. Epäonnistuneen funktiokutsun jälkeen emme halua lisää funktiokutsuja.

Tyypille `Either` on määritelty tyyppiluokan `Functor` instanssi samaan tapaan.

```haskell
instance Functor (Either a) where
  fmap f (Right x) = Right (f x)
  fmap f (Left x) = Left x
```

## Tyyppiluokka `Monad`

Tyyppiluokka `Monad` määrittelee funktiot `(>>=)` (*bind*) ja `return`. 

```haskell
class  Monad m  where
    (>>=)   :: m a -> (a -> m b) -> m b
    (>>)    :: m a -> m b -> m b
    return  :: a -> m a
    fail    :: String -> m a

    m >> k  =  m >>= \_ -> k
    fail s  = error s
```

Luokkaan kuuluva funktio `(>>)` (*then*) on funktion `(>>=)` erikoistapaus. Funktio `fail` toimii operaation virhevaihtoehtona, kun puramme monadiarvon muuttujaan sijoitusnuolen `<-` avulla.

Haskell kielen `do`-lauseke on lyhentävä ja ohjelman luettavuutta parantava merkintätapa monadifunktioille `(>>)` ja `(>>=)`.

Lauseke

```haskell
do 
  e1
  e2 
```

vastaa lauseketta

```haskell
e1 >> e2
```  

ja lauseke

```haskell
do 
  p <- e1
  e2
```

lauseketta

```haskell
e1 >>= \p -> e2
```

Virhemahdollisuus huomioiden se on sama kuin lauseke

```haskell
e1 >>= (\v -> case v of 
  p -> e2
  _ -> fail "s") 
```

Saamme esimerkiksi

```haskell
> e1 = print "e1"
> e2 = print "e2"
> do e1; e2
"e1"
"e2"
> e1 >> e2
"e1"
"e2"
```

Lauseke 

```haskell
do 
  p <- getLine; 
  print p
```

puolestaan merkitsee samaa kuin

```haskell
getLine >>= (\p -> print p)
```

Monadi `Maybe` määrittelee funktiot `return`, `(>>=)` ja `fail` seuraavasti:

```haskell
instance Monad Maybe where
  return :: a -> Maybe a
  return = Just
 
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  (Just x) >>= g = g x
  Nothing  >>= _ = Nothing

  fail _ = Nothing
```

Yritämme seuraavassa sovittaa monadiarvon sijoitusnuolen `<-` avulla arvoja `[2,1]`, `[1]` ja `[]` parametrimuotoon `(x:xs)`.

```haskell
just2 = do (x:xs) <- Just [2,1]; return x 
just1 = do (x:xs) <- Just [1]; return x 
fail1 = do (x:xs) <- Just []; return x 
```

Nyt funktiot `just2`, `just1` ja `fail1` palauttavat arvot `Just 2`, `Just 1` ja `Nothing`.

```haskell
> just2
Just 2
> just1
Just 1
> fail1
Nothing
```

Listamonadi `([])` määrittelee funktiot `return`, `(>>=)` ja `fail` seuraavasti:

```haskell
instance Monad [] where  
  return x = [x]  
  xs >>= f = concat (map f xs)  
  fail _ = [] 
```

Funktioiden `map` ja `concat` valintaa bind-funktion `(>>=)` toteutukseen sekä funktioiden `return` ja `fail` paluuarvojen valintaa voisimme perustella esimerkiksi niiden käyttökelpoisuudella listamuodostimien rakennusosina.

```haskell
> map (\x -> if even x then [x] else []) [1..6]
[[],[2],[],[4],[],[6]]
> concat [[],[2],[],[4],[],[6]]
[2,4,6]
```

Tämä vastaa jotakuinkin listamuodostinta


```haskell
> [x | x <- [1..6], even x]
[2,4,6]
```

Vuorovaikutteisen tulkin komennolla `:info` saamme selville, että listat `([])` ovat muun muassa tyyppiluokkien `Monad`, `Functor`, `Applicative` ja `Monoid` jäseniä. 

```haskell
> :info []
data [] a = [] | a : [a]
...
instance Monad []
instance Functor []
instance Applicative []
instance Monoid [a]
...
```

Koska listat ovat monadi, voimme monadiarvon sijoitusnuolen `<-` avulla purkaa muuttujan monadista (ja lisätä sen vaikkapa takaisin funktion `return` avulla).

Lauseke

```haskell
do 
  x <- [1..4]
  return x
```

palauttaa siten arvon `[1,2,3,4]`.

```haskell
> do x <- [1..4]; return x
[1,2,3,4]
```

## Tyyppiluokka `Num`

Numeeristen tyyppiluokkien laajin luokka on tyyppiluokka `Num`, johon kuuluvat kaikki numeeriset tyypit. 

Esimerkiksi literaali `5` on oletuksena tyyppiluokan `Num` arvon esitystapa.

```haskell
> :t 5
5 :: Num t => t
```

Tyyppiluokassa `Num` on määritelty funktiot `(+)`, `(-)`, `(*)`, `negate`, `abs`, `signum` ja `fromInteger`.

## Tyyppiluokka `Real`

Tyyppiluokassa `Real` on määritelty funktio `toRational`. Luokkaan kuuluvat lukutyypit `Word`, `Integer`, `Int`, `Float` ja `Double`.

## Tyyppiluokka `Integral`

Tyyppiluokka `Integral` on kokonaislukutyyppien `Int`, `Word` ja `Integer` luokka. Luokka määrittelee kokonaisjakofunktiot `quot`, `rem`, `div`, `mod`, `quotRem` ja `divMod` sekä funktion `toInteger`.

## Tyyppiluokka `RealFrac`

Tyyppiluokkaan `RealFrac` kuuluvat liukulukutyypit `Float` ja `Double`. Luokka määrittelee muun muassa funktiot `truncate`, `round`, `ceiling` ja `floor`.

## Tyyppiluokka `Floating`

Tyyppiluokkaan `Floating` kuuluvat liukulukutyypit `Float` ja `Double`. Luokka määrittelee matemaattiset funktiot `pi`, `exp`, `log`, `sqrt`, `(**)`, `logBase`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `asinh`, `acosh`, ja `atanh`.

## Tyyppiluokka `Fractional`

Tyyppiluokka `Fractional` määrittelee funktiot `(/)`, `recip` ja `fromRational`. Luokkaan kuuluvat liukulukutyypit `Float` ja `Double`.


