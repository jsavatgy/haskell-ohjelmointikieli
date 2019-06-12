# Tyypillisiä funktiorakenteita

## Yhdistetty funktio

Voimme kirjoittaa funktion `f (g (x))` muodossa `(f . g) x`. Nimitämme funktiota `f . g` *yhdistetyksi funktioksi*. Määrittelemme yhdistetyn funktion operaation `(.)` seuraavasti:

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)
```

Yhtäpitävän vaihtoehtoisen määrittelyn saamme myös nimettömänä funktiona

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)
```

## Funktio `map`

Funktion `map` tehtävä on soveltaa annettua *kuvausfunktiota* listan alkioihin. Esittelemme funktion `map` toteutuksen listamuodostimilla ja vaihtoehtoisesti rekursion avulla.

```haskell
map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs
```

## Funktio `filter`

Suodatusfunktio `filter` kokoaa listasta annetun ehdon täyttävät alkiot. Myös tämän funktion voimme toteuttaa listamuodostimilla tai rekursiolla.

```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter p xs = [x | x <- xs, p x]

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs) | p x = x : filter p xs
                | otherwise = filter p xs
```

## Funktiot `foldr` ja `foldl`

Määrittelemme seuraavaksi niin sanotut *taittelufunktiot* `foldl` ja `foldr`. 
Nimet `foldl` (*fold-left*, "taittele vasemmalle") ja `foldr` (*fold-right*, "taittele oikealle") noudattavat kuvan \ref{fig:folded} periaatetta.
Sanomme *taiteltavaksi* (tyyppiluokka `Foldable`) tietorakennetta, joka on mahdolllista järjestää läpikäyntiä varten kuvan esittämään muotoon. 
Käytännössä taittelu on rekursiivinen operaatio, joka kohdistuu kerrallaan kahteen alkioon. 

```haskell
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f a [] = a                  
foldl f a (x:xs) = foldl f (f a x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f a [] = a
foldr f a (x:xs) = f x (foldr f a xs)
```

Tässä funktio `f` on taitteluoperaatio ja alkio `a` operaation alkuarvo eli nolla-alkio. Kuvassa \ref{fig:folded} olemme esittäneet graafisesti lausekkeet `foldl (+) 0 [1..5]` ja `foldr (+) 0 [1..5]`.

\begin{figure}[H]
\begin{center}
\includegraphics{folded.pdf}
\caption{Lausekkeet \texttt{foldl (+) 0 [1..5]} ja \texttt{foldr (+) 0 [1..5]}.}
\label{fig:folded}
\end{center}
\end{figure}

Molempien lausekkeiden arvoksi saamme 15.

```haskell
> foldl (+) 0 [1..5]
15
> foldr (+) 0 [1..5]
15
```

Funktiot `foldr` ja `foldl` eivät aina toimi symmetrisesti. Esimerkiksi listakonstruktorin toinen parametri on alkio ja toinen lista, joten voimme taitella listan listakonstruktorin suhteen ainoastaan oikealle (kuva \ref{fig:fold-list}).

\begin{figure}[H]
\begin{center}
\includegraphics{fold-list.pdf}
\caption{Lauseke \texttt{foldr (:) [] [1..4]}.}
\label{fig:fold-list}
\end{center}
\end{figure}

Saamme esimerkiksi

```haskell
> foldr (:) [] [1..4]
[1,2,3,4]
```

Voisimme määritellä listojen yhteydessä käyttämämme funktion `concat` funktion `foldr` avulla parametrittomassa muodossa varsin ytimekkäästi.

```haskell
concat :: [[a]] -> [a]
concat = foldr (++) []
```

## Funktiot `foldr1` ja `foldl1`

Voimme määritellä funktioista `foldl` ja `foldr` helppokäyttöisemmät muodot `foldr1` ja `foldl1`, jotka käyttävät listan ensimmäistä alkiota alkuarvona.

```haskell
foldl1           :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs)  =  foldl f x xs
foldl1 _ []      =  error "ERROR: empty list."

foldr1           :: (a -> a -> a) -> [a] -> a
foldr1 f [x]     =  x
foldr1 f (x:xs)  =  f x (foldr1 f xs)
foldr1 _ []      =  error "ERROR: empty list."
```

Saamme nyt 

```haskell
> foldl1 (+) [1,2,3,4]
10
> foldr1 (+) [1,2,3,4]
10
```

Havaitsemme, että funktion `foldr1` parametri `f` on funktio tyyppiä `a -> a -> a` ja lista `x:xs` tyyppiä `[a]`, joten emme voi ajatella käyttävämme listakonstruktorifunktiota `(:) :: a -> [a] -> [a]` funktiona `f`.

## Ketjutus

Olemme aiemmin puhuneet yhden, kahden ja kolmenkin parametrin funktioista. Täsmällisesti ottaen Haskell-kielessä ei ole kahden saati kolmen parametrin funktioita. On vain yhden parametrin funktio, joka muodostaa uuden funktion, joka voi saada uuden argumentin, joka edelleen voi saada uuden argumentin, ja niin edelleen, muodostaen näin ihmisen silmissä useamman parametrin funktion. Tästä prosessista, jota nimitämme *ketjutukseksi*, kertoo tyyppimäärittelyn nuoli `->`, joka symbolisoi matematiikan funktion kuvauksen merkintätapaa.

```haskell
f :: Int -> Int -> Int
f x y = x + y

g :: Int -> (Int -> Int)
(g x) y = x + y
```

Tässä funktiot `f` ja `g` ovat samoja. Vuorovaikutteinen tulkki kertoo nyt funktioiden `f`, `f 1` ja `f 1 2` tyypit.

```haskell
> :t f
f :: Int -> Int -> Int
> :t f 1
f 1 :: Int -> Int
> :t f 1 2
f 1 2 :: Int
```

Jos nyt määrittelemme funktion `f 1` uudella nimellä `plusOne`, saamme funktion, joka odotuksiemme mukaisesti palauttaa luvun, joka on yhden argumenttiaan suurempi. 

```haskell
> plusOne = f 1
> plusOne 2
3
```

Funktio voi saada parametrinaan vektorityyppisen muuttujan, esimerkiksi parin `(x,y)` tai kolmikon `(x,y,z)`. Mikäli kuitenkin mahdollista, pyrimme välttämään muuttujien tarpeetonta pakkaamista pareiksi ja kolmikoiksi.

```haskell
f :: (Int,Int) -> Int
f (x,y) = x + y
```

## Nimettömät funktiot

Nimetön funktio on muotoa `\x -> f x`. Luemme tämän muodossa "*lambda x* palauttaa arvon *f x*". Lambda, $\lambda$, on kreikkalaisten aakkosten yhdestoista kirjain, jota usein käytetään funktionaalisten kielten symbolina.  

Funktio `\x -> 2 * x` on yhden parametrin nimetön funktio, kun taas funktio `\x y -> x + y` on kahden parametrin nimetön funktio.

Käytämme nimettömiä funktioita usein funktioiden `filter` ja `map` yhteydessä tai parametrien järjestystä vaihdettaessa.

```haskell
> filter (\x -> 3 <= x && x <= 5) [1..9] 
[3,4,5]
> filter (\x -> snd x `mod` 2 == 1 && fst x <= 'e') 
         (zip ['a'..'z'] [1..])
[('a',1),('c',3),('e',5)]
> map (\x -> x `max` 0) [-1..2]
[0,0,1,2]
> map (\x -> x `mod` 2) [1..4]
[1,0,1,0]
> map (\x -> (x,x))  [1..3]
[(1,1),(2,2),(3,3)]
```

Samaan tapaan voimme siirtää funktiomäärittelyn parametrit yhtälön toiselle puolelle.

```haskell
add x y = x + y
add x = \y -> x + y
add = \x y -> x + y
```

## Funktion osittainen toteutus

Nimettömien funktioiden avulla voimme määritellä infix-muotoisille funktioille *osittaisen toteutuksen*.

```haskell
(x +) = \y -> x + y
(+ y) = \x -> x + y
(+) = \x y -> x + y
```

Käytimme funktioiden osittaistoteutuksia aikaisemmin `map`-funktion yhteydessä.

```haskell
> map (2 +) [1,2,3] 
[3,4,5]
> map (3 *) [1,2,3] 
[3,6,9]
> map (/ 2) [2,4,8] 
[1.0,2.0,4.0]
> map ('O' :) ["blivionis","dii","rientale"]
["Oblivionis","Odii","Orientale"]
> map (++ "um") ["Humor","Undar","Vapor"]
["Humorum","Undarum","Vaporum"]
```

## Tietueet ja ketjutus

Määrittelemme listan `t`, jonka alkioita ovat kokonaisluvun ja kirjaimen parit.

```haskell
> t = zip [1..5] ['a'..]
> t
[(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e')]
```

Kirjain on tyyppiä `Char`, ja voimme muuttaa sen merkkijonoksi tyyppiä `String` yksinkertaisesti listakonstruktorilla `[]`.

```haskell
> ['a']
"a"
```

Määrittelemme funktion `g` palauttamaan parin alkiot, eli luvun ja kirjaimen. Kuvaamme koko listan `t` funktion `g` määrittelemään tulostusmuotoon.

```haskell
> t = zip [1..5] ['a'..]
> g (a,b) = show a ++ " " ++ [b]
> g (1,'a')
"1 a"
> map g t
["1 a","2 b","3 c","4 d","5 e"]
```

Nimettömän funktion `\(a,b) -> f a b` avulla voimme kutsua muodossa `f a b` olevaa funktiota parametrilla `(a,b)`. 

```haskell
> f a b = show a ++ " " ++ [b]
> f 1 'a'
"1 a"
> map (\(a,b) -> f a b) t
["1 a","2 b","3 c","4 d","5 e"]
```

Samaan tarkoitukseen löydämme standardikirjastosta `Prelude` funktion `uncurry`.

```haskell
> t = zip [1..5] ['a'..]
> f a b = show a ++ " " ++ [b]
> map (uncurry f) t
["1 a","2 b","3 c","4 d","5 e"]
```

Vastakkaissuuntaisen muunnoksen suorittaisimme funktiolla `curry`. Funktioiden `curry` ja `uncurry` tyyppimäärittelyt ovat

```haskell
> :t curry
curry :: ((a, b) -> c) -> a -> b -> c
> :t uncurry
uncurry :: (a -> b -> c) -> (a, b) -> c
```

Funktio `curry` muuntaa ketjuttamattoman funktion ketjutetuksi funktioksi ja funktio `uncurry` ketjutetun funktion ketjuttamattomaksi. Ketjutettu funktio operoi kahdella parametrilla, ketjuttamaton yhdellä tietuetyypin parametrilla, jolla on kaksi alkiota.

Vastaavasti voimme muodostaa haluamamme parit suoraan funktiolla `zipWith`.

```haskell
> zipWith f [1..5] ['a'..]
["1 a","2 b","3 c","4 d","5 e"]
```

## Assosiaatio ja laskujärjestys

Voimme määritellä laskutoimitusten assosiaation ja suoritusjärjestyksen. Mikäli määrittelyt ovat annetut ja ne poikkeavat oletuksesta, saamme tiedot vuorovaikutteisen tulkin komennolla `:info`. Esimerkiksi yhteenlaskuoperaatiolle saamme

```haskell
> :info (+)
class Num a where
  (+) :: a -> a -> a
infixl 6 +
```

Standardikirjasto määrittelee muun muassa seuraavat assosiaatio- ja laskujärjestyksen prioriteettiluokat:

```haskell
infixr 9  .
infixr 8  ^, ^^, **
infixl 7  *, /, `quot`, `rem`, `div`, `mod`
infixl 6  +, -
infixr 5  :
infix  4  ==, /=, <, <=, >=, >
infixr 3  &&
infixr 2  ||
```

Määrittelemme vasemmalle assosioivat laskutoimitukset komennolla `infixl` ja oikealle assosioivat komennolla `infixr`. Laskutoimitukset, joilta assosiaatio puuttuu, määrittelemme komennolla `infix`. Funktiokutsun sitovuus suhteessa parametriin kuuluu prioriteettiluokkaan 10. Tähän luokkaan emme voi määritellä omia funktioitamme. Tavallisesti emme määrittele omille funktioillemme erikseen prioriteettiluokkaa, jolloin ne oletuksena kuuluvat luokkaan `infixl 9`.

Laskujärjestysmäärittelyiden perusteella voimme suluttaa lausekkeet seuraavasti:

```haskell
> 2 + 3 * 4 == 2 + (3 * 4)
True
> 3 - 2 - 1 == (3 - 2) - 1
True
> 2 ^ 2 ^ 3 == 2 ^ (2 ^ 3)
True
```

## Funktiot `(**)`, `(^)` ja \texttt{(\^{}\^{})}

Funktiot `(**)`, `(^)` ja `(^^)` ovat kaikki potenssiinkorotusoperaatioita. 

Funktio `(**)` on määritelty yleisesti liukuluvuille.

```haskell
> :type (**)
(**) :: Floating a => a -> a -> a
```

Funktio `(^)` on määritelty ei-negatiivisille kokonaispotensseille.

```haskell
> :type (^)
(^) :: (Num a, Integral b) => a -> b -> a
```

Funktio `(^^)` on määritelty funktioiden `(^)`, `recip` ja `negate` avulla yleisesti kokonaispotensseille.

```haskell
x ^^ n =  if n >= 0 then x^n else recip (x^(negate n))
> :type (^^)
(^^)  :: (Fractional a, Integral b) => a -> b -> a
```

Negatiivisten potenssien laskusääntö on matematiikasta tuttu $$a^{-n} = \dfrac{1}{n}$$

## Funktio `($)`

Määrittelemme funktion `($)`

```haskell
infixr 0 $
($) :: (a -> b) -> a -> b
f $ x = f x
```

Suodatamme nyt listasta `[1..10]` parilliset alkiot funktiolla `filter even`, käännämme saadun listan ympäri ja palautamme kolme ensimmäistä alkiota.

```haskell
> take 3 (reverse (filter even [1..10]))
[10,8,6]
```

Määrittelemme funktiot `f`, `g` ja `h`

```haskell
f = take 3
g = reverse
h = filter even
k = f . g . h
```

Muistamme aiempaa kuinka funktiot `take`, `reverse` ja `filter` toimivat.

```haskell
> f [1..10]
[1,2,3]
> g [1..10]
[10,9,8,7,6,5,4,3,2,1]
> h [1..10]
[2,4,6,8,10]
```

Näin ollen saamme

```haskell
> take 3 (reverse (filter even [1..10]))
[10,8,6]
> (take 3 . reverse . filter even) [1..10]
[10,8,6]
> (f . g . h) [1..10]
[10,8,6]
> k [1..10]
[10,8,6]
```

Funktion `($)` avulla voimme nyt kirjoittaa

```haskell
> k $ [1..10]
[10,8,6]
> f . g . h $ [1..10]
[10,8,6]
> take 3 . reverse . filter even $ [1..10]
[10,8,6]
```

Vaikka funktiomuoto `(f . g . h) [1..10]` on ulkoasultaan selvästi kauniimpi, on muoto `f . g . h $ [1..10]` haskellistien suosima.

Tarkastelemme vielä miten laskujärjestys ja assosiatiivisuus vaikuttaa funktion `g . h $ [1..10]` suoritukseen. Oikealta assosioiva funktio `($)` toimii kaikkein alhaisimmalla prioriteetilla 0 odotetusti. Sen sijaan funktion `(.)` kanssa samalla prioriteetilla määritettynä seurauksena on funktion tyyppivirhe.

```haskell
> infixr 0 $; f $ x = f x
> g . h $ [1..10]
[10,8,6,4,2]
> infixr 9 $; f $ x = f x
> g . h $ [1..10]
"ERROR: Couldn't match type ‘[a0]’ with ‘a -> [a1]’."
```

Vasemmalta assosioivana funktio toimii samoin, mutta virheilmoitus on erilainen. Se kertoo laskujärjestyksen olevan mahdoton. Lausekkeen sulutus olisi tässä tapauksessa limittäinen. 

```haskell
> infixl 0 $; f $ x = f x
> g . h $ [1..10]
[10,8,6,4,2]
> infixl 9 $; f $ x = f x
> g . h $ [1..10]
"ERROR: Precedence parsing error."
```

Missään tapauksessa laskujärjestyksen ja assosiatiivisuuden määrittely ei kumoa erikseen merkittyä sulutusta. Samoin seuraavassa yhdistetyn funktion `g . h` sitominen funktion `m` määrittelyyn nostaa sen prioriteetin automaattisesti muiden määrittelyjen ohitse. 

```haskell
> m = g . h
> g (h $ [1..10])
[10,8,6,4,2]
> m $ [1..10]
[10,8,6,4,2]
```

Muistamme, kuinka käytimme funktion `(++)` osittaistoteutusta merkkijonoille.

```haskell
> (++ "um") "Vapor"
"Vaporum"
```

Koska määrittelimme `f $ x = f x`, voimme nyt käyttää funktion `($)` osittaistoteutusta vastaavalla tavalla.

```haskell
> ($ 3) (4 +) 
7
```

Funktion `map` avulla laajennamme toimenpiteen listoille.

```haskell
> map (++ "um") ["Vapor","Undar","Humor"]
["Vaporum","Undarum","Humorum"]
> map ($ 3) [(4 +),(3 *),(2 ^)]
[7,9,8]
```


