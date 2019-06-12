# Tietotyypit

## Binäärijärjestelmä tietotyyppien perustana

Kutsumme *bitiksi* pienintä tiedon esittämisen yksikköä, joka voi sisältää yhden kahdesta mahdollisesta arvostaan. Voimme tulkita nämä arvot luvuiksi 0 ja 1, totuusarvoiksi `False` ja `True`, tai jollakin muulla haluamallamme tavalla. Tässä kappaleessa tulkitsemme ne merkeiksi `'0'` ja `'1'`.

Yhden bitin avulla voimme esittää kaksi mahdollista arvoa, kahden bitin avulla neljä, kolmen bitin avulla kahdeksan, neljän bitin avulla kuusitoista, ja niin edelleen. Bittien määrän ollessa `n`, saamme niiden avulla esitettävissä olevien arvojen määrän funktiolla `f n = 2 ^ n`.

```haskell
> f n = 2 ^ n
> map f [0..8]
[1,2,4,8,16,32,64,128,256]
```

Ennen kuin lähdemme tarkastelemaan teoreettisella tasolla varsinaisia tietotyyppejä, syvennämme hieman Haskell-kielen osaamistamme tyypillisten funktioiden parissa ja tutustumme merkkijonoaritmetiikan avulla bittijonojen muodostumiseen.

Kuten kymmenjärjestelmä, myös binäärijärjestelmä soveltuu *laskemiseen*. Voimme pyytää Haskell-kieltä laskemaan määrittelemiemme binäärijärjestelmän merkkien `'0'` ja `'1'` avulla. Määrittelemme tätä varten rekursion avulla funktion `toBinary`.


```haskell
toBinary 0 = "0"
toBinary 1 = "1"
toBinary r = toBinary (r `div` 2) ++ 
  if (r `mod` 2 == 1) then "1" else "0"
```

Nyt saamme

```haskell
> map toBinary [0..15]
["0","1","10","11","100","101","110","111","1000","1001",
 "1010","1011","1100","1101","1110","1111"]
```

Vaihtoehtoisesti voimme käyttää kirjaston `Text.Printf` funktiota `printf`.

```haskell
> import Text.Printf
> inBinary i = printf 
  "The value of %d in binary is: %08b\n" i i
> inBinary 5
The value of 5 in binary is: 00000101
```

## Totuusarvotyyppi `Bool`

Yhden bitin avulla toteutettavista tietotyypeistä mainitsimme edellä totuusarvotyypin `Bool`. Se voi saada toisen arvoista `False` tai `True`.

```haskell
> :t False
False :: Bool
> :t True
True :: Bool
> not False
True
```

Totuusarvotyypin `Bool`  konstruktorit `False` ja `True` ovat Haskell-kielen esitystapa arvoille. Kääntäjä muuntaa ne tietokoneen sisäiseen esitystapaan, biteiksi, ohjelman käännöksen yhteydessä. Voimme määritellä totuusarvotyypin `Bool` halutessamme itse seuraavasti:

```haskell
data Bool = False | True
```

## Merkkityyppi `Char`

Jo tietokoneiden alkuajoista lähtien bitit koottiin bittijonoiksi, jotka johdettiin rinnakkain niin sanottua *väylää* pitkin prosessoriin. Ensimmäisissä kaupallisissa tietokoneissa väylän leveys oli kahdeksan bittiä. Nykyaikaiset prosessorit ovat pääosin 64-bittisiä. 

Tietokoneiden historia on vaikuttanut merkittävästi ohjelmointikielten tietotyyppien koon määräytymiseen. Kahdeksan bitin järjestelmä säilyi hallitsevana vuosikymmenet. Ehkä suurin vaikutus tällä on ollut tietokoneen käyttämään merkistöön. Tänä päivänä Haskell-kieli käyttää kohtuu sujuvasti laajempaa Unicode-merkistöä, mutta 8-bittisen (aluksi 7 bittiä ja tarkistusbitti) ASCII-merkistön vaikutus on yhä nähtävissä esimerkiksi kielen syntaksissa.

Kahdeksalla (tai seitsemällä) bitillä saamme esitettyä latinan suur- ja pienaakkoset.

```haskell
> (['A'..'Z'],['a'..'z'])
("ABCDEFGHIJKLMNOPQRSTUVWXYZ",
 "abcdefghijklmnopqrstuvwxyz")
```

Näistä `'A'` on saanut ASCII-merkistössä järjestysluvun 65 ja `'a'` järjestysluvun 97. Bittitasolla muunnos latinan suuraakkosten ja pienaakkosten välillä onnistuu yhtä bittiä muuttamalla.

```haskell
> ord 'A'
65
> ord 'a'
97
> inBinary (ord 'A')
The value of 65 in binary is: 01000001
> inBinary (ord 'a')
The value of 97 in binary is: 01100001
```

Yleinen merkin tietotyyppi Haskell-kielessä on `Char`. Tämä tietotyyppi pitää sisällään kaikki Unicode-merkistön merkit, myös sellaiset, jotka eivät kuulu alkuperäiseen 7-bittiseen ASCII-merkistöön. 

Kirjastossa `Data.Char` on määritelty funktio `isAscii`, joka palauttaa totuusarvon `TRUE`, mikäli merkki kuuluu ensimmäiseen 127 merkin joukkoon. Vastaavasti funktio `isLatin1` palauttaa totuusarvon `TRUE`, mikäli merkki kuuluu laajennettuun ASCII-merkistöön eli ensimmäiseen 255 merkin joukkoon.

```
isAscii :: Char -> Bool
isAscii c = c < '\128'

isLatin1 :: Char -> Bool
isLatin1 c = c <= '\255'
```

Esimerkiksi pohjoismaisten kielten kirjain `'æ'` on järjestysluvultaan 230, eikä siten kuulu 7-bittiseen ASCII-merkistöön, mutta kuuluu laajennettuun Latin1-merkistöön.

```
> import Data.Char
> ord 'æ'
230
> isAscii 'æ'
False
> isLatin1 'æ'
True
```

## Funktiot `minBound` ja `maxBound`

Funktioiden `minBound` ja `maxBound` avulla saamme selville tyyppiluokkaan `Bounded` kuuluvan tyypin ala- ja ylärajan. Nämä funktiot eivät saa parametreja, mutta käyttäytyvät eri tavalla tyyppiluokan sisällä tyypistä riippuen. Koska funktioiden on tiedettävä tyyppinsä, ilmoitamme tyypin tyyppimäärittelymerkinnän `::` avulla

```haskell
> minBound :: Bool
False
> maxBound :: Bool
True
> minBound :: Char
'\NUL'
> maxBound :: Char
'\1114111'
```

## Kokonaislukutyyppi `Word`

Kahdeksalla bitillä voimme esittää positiiviset kokonaisluvut `0..255`. Vastaavasti kuudellatoista bitillä voimme esittää luvut `0..65535`. Löydämme tähän tarvittavat tietotyypit kirjastosta `Data.Word`. 

Perustyyppi `Word` kuuluu standardikirjastoon `Prelude`. Haskell-kääntäjä valitsee sen koon automaattisesti prosessorin väylänleveyden mukaisesti.

```haskell
> import Data.Word
> minBound :: (Word8,Word16,Word32,Word64)
(0,0,0,0)
> maxBound :: (Word8,Word16,Word32,Word64)
(255,65535,4294967295,18446744073709551615)
> maxBound :: Word
18446744073709551615
```

## Kokonaislukutyyppi `Int`

Mikäli haluamme mukaan negatiiviset luvut, joudumme varaamaan yhden bitin etumerkille. Löydämme vastaavat tietotyypit kirjastosta `Data.Int`. 

Perustyypin `Int` kääntäjä valitsee automaattisesti. 

```haskell
> import Data.Int
> minBound :: (Int8,Int16,Int32,Int64)
(-128,-32768,-2147483648,-9223372036854775808)
> maxBound :: (Int8,Int16,Int32,Int64)
(127,32767,2147483647,9223372036854775807)
> minBound :: Int
-9223372036854775808
> maxBound :: Int
9223372036854775807
```

## Rajoittamaton kokonaislukutyyppi `Integer`

Koska listojen koko ei Haskell-kielessä ole rajoitettu, voimme (käytettävissä olevan muistin rajoissa) esittää niiden avulla miten suuria kokonaislukuja tahansa. Luonnollisesti lukuaritmetiikka on tällöin huomattavasti hitaampaa. Haskell-kielessä voimme tällaisia rajoittamattomia kokonaislukuja esittää tietotyypin `Integer` avulla.

```haskell
> m = 9223372036854775807
> zipWith ($) [(+0),(+1),(+2)] (repeat (m :: Integer))
[ 9223372036854775807,
  9223372036854775808,
  9223372036854775809 ]
> zipWith ($) [(+0),(+1),(+2)] (repeat (m :: Int))
[ 9223372036854775807,
 -9223372036854775808,
 -9223372036854775807 ]
```

Esimerkissä näemme tietotyypin `Int` *ylivuodon*. Luonnollisesti tietotyypin yläraja on niin suuri, että käytännön sovelluksissa tietotyyppi `Int` on täysin riittävä. 

## Desimaalityypit `Float` ja `Double`

Desimaalilukujen esittäminen binäärijärjestelmässä ei ole täysin yksikäsitteistä. Käytännössä usein esiintyviä desimaalityyppejä ovat Haskell-kielessä "yksinkertaisen" tarkkuuden liukulukutyyppi `Float` ja "kaksinkertaisen" tarkkuuden liukulukutyyppi `Double`.

```haskell
> tau = 6.28318530717958647692
> tau :: Float
6.2831855
> tau :: Double
6.283185307179586
```

## Luetellut tyypit

Luetelluista tyypeistä olemme tähän mennessä jo tutustuneet tyyppiin `Bool`.

```haskell
data Bool = False | True
```

Vastaavalla tavalla voimme muodostaa luetellun tyypin viikonpäivistä, shakkipelin nappuloista tai korttipelin korteista.

```haskell
data Weekday = 
  Monday | Tuesday | Wednesday | Thursday | Friday | 
  Saturday | Sunday
data Piece = 
  King | Queen | Bishop | Knight | Rook | Pawn
data CardValue = 
  Two | Three | Four | Five | Six | Seven | Eight | 
  Nine | Ten | Jack | Queen | King | Ace
```

Kuten muistamme, korttipakassa on neljä maata: risti, ruutu, hertta ja pata.

```haskell
data Suit = Club | Diamond | Heart | Spade
```

Voimme nyt esittää korttipakan kortit tietotyypin `Card` avulla

```haskell
data Card = Card Suit CardValue
```

Tässä vasemman puolen `Card` on tyypin nimi eli tyyppikonstuktori, oikean puolen `Card` kahden parametrin arvokonstruktori. Tyypit `Suit` ja `CardValue` ovat arvokonstruktorin `Card` parametreja. Arvokonstruktorin `Card` avulla saamme herttakasista, patakympistä ja ruutuseiskasta tyypin `Card` arvoja.

```haskell
> :t Card
Card :: Suit -> CardValue -> Card
> :t Card Heart
Card Heart :: CardValue -> Card
> :t Card Heart Eight
Card Heart Eight :: Card
> :t Card Spade Ten
Card Spade Ten :: Card
> :t Card Diamond Seven
Card Diamond Seven :: Card
```

Erilaisia kortin arvoja on 13. Saamme ne laskemalla yhteen tyypin `CardValue` konstruktorien määrän. Tämän vuoksi nimitämme tyyppiä `CardValue` *summatyypiksi*.

Korttipakassa kortteja on 52. Saamme ne kertomalla keskenään tyyppien `Suit` ja `CardValue` konstruktorien lukumäärän. Nimitämme tyyppiä `Card` *tulotyypiksi*.

## Abstraktit tyypit

Nimitämme *abstraktiksi tyypiksi* tyyppiä, joka sisältää tyyppimuuttujia. Haskell-kielessä kirjoitamme tyyppimuuttujat pienellä alkukirjaimella. Abstrakti tyyppi voi toteutuksessa olla mikä tahansa tyyppi.

Yksinkertainen esimerkki tyyppimuuttujasta on identiteettifunktio `id`. Funktio `id` palauttaa saman arvon, jonka se saa argumenttinaan. 

```haskell
> id x = x
> id 4
4
```

Funktiokutsun `id x` parametri `x` on abstraktia tyyppiä `a`, eli se voi olla mitä tahansa tyyppiä `a`. Funktion palautusarvo on samaa tyyppiä `a`.

```haskell
> :type id
id :: a -> a
```

Käyttötarkoituksesta riippuen tyyppimuuttujalla voi olla rajoitteita. Esimerkiksi funktio `max` vaatii parametreiltaan järjestysominaisuutta (*Orderable*) ja funktio `(+)` vaatii, että molemmat argumentit ovat numeerisia (*Numeric*).

```haskell
> :t max
max :: Ord a => a -> a -> a
> :t (+)
(+) :: Num a => a -> a -> a
```

Funktion `length` parametri on lista tyyppiä `[t1]`. Tyyppi `t1` voi olla mikä tahansa tyyppi. Funktion palautusarvo on numeerinen. 

```haskell
> :t length
length :: Num t => [t1] -> t
```

Funktion `length` palautusarvot ovat kokonaislukuja. Palautusarvon tyypin määritteleminen tyyppimuuttujan `t` avulla tarjoaa kuitenkin mahdollisuuden käyttää funktion palautusarvoa edelleen lausekkeissa, joiden arvo ei välttämättä ole kokonaisluku. Esimerkiksi jakolasku tiukentaa numeerisuuden vaatimusta osamäärämuotoiseksi (*Fractional*).

```haskell
> length [1..9] / 2
4.5
> :t length [1..9] / 2
length [1..9] / 2 :: Fractional a => a
```

## Rekursiiviset tyypit

Olemme aiemmin käyttäneet rekursiivisista tyypeistä muun muassa listoja. Määrittelemme listat nyt eri konstruktorinimiä käyttäen.

```haskell
data List a = Nil | Cons a (List a)
```

Tässä vasemman puolen `List` on tyypin nimi eli tyyppikonstruktori. Yhdessä tyyppimuuttujan `a` kanssa se muodostaa tyypin. Tyyppimuuttuja `a` voi olla mikä tahansa tyyppi, kuten `Int`, `Float`, `Bool`, `Char` tai `String`. Esimerkiksi lausekkeen `Cons 'a' (Cons 'b' Nil)` tyyppi  on `List Char`.

Tyyppi `List a` on rekursiivinen, sillä konstruktori `Cons` saa ensimmäisenä parametrina alkion tyyppiä `a` ja toisena parametrina tyyppiä `List a` olevan listan.

```haskell
> :t Nil
Nil :: List a
> :t Cons 3 (Cons 2 Nil)
Cons 3 (Cons 2 Nil) :: Num a => List a
> :t Cons 'a' (Cons 'b' Nil)
Cons 'a' (Cons 'b' Nil) :: List Char
> :t Cons "A." (Cons "B." Nil)
Cons "A." (Cons "B." Nil) :: List [Char]
> :t Cons True (Cons False Nil)
Cons True (Cons False Nil) :: List Bool
```

## Tyyppikonstruktori `Maybe`

Tyypin `Maybe a` mahdollisia arvoja ovat `Nothing` ja `Just a`. Jälleen tyyppimuuttuja `a` voi olla mikä tahansa tyyppi. Yhdessä konstruktorin `Just` kanssa se muodostaa alkion tyyppiä `Maybe a`.

```haskell
data Maybe a = Nothing | Just a
```

Tyyppimuuttuja `a` voi olla esimerkiksi rajoitettu kokonaislukutyyppi `Int`.

```haskell
> b = 2 :: Int
> Just b
Just 2
> :t Just b
Just b :: Maybe Int
```

Tyypin `Maybe a` yleinen käyttötarkoitus on valintatilanne, jossa arvo joko on olemassa (`Just x`) tai sitä ei ole olemassa (`Nothing`). Esimerkiksi etsittäessä alkiota listasta, standardikirjaston funktio `lookup` palauttaa arvon `Just x`, kun avain löytyy listasta ja arvon `Nothing`, kun avainta ei löydy listasta.

```haskell
> lookup 'c' [('a',0),('b',1),('c',2)]
Just 2
> lookup 'd' [('a',0),('b',1),('c',2)]
Nothing
> :t lookup
lookup :: Eq a => a -> [(a, b)] -> Maybe b
```

Funktion `lookup` tyyppiallekirjoitus kertoo, että avaimen täytyy kuulua tyyppiluokkaan `Eq`. Tämä on ymmärrettävä vaatimus, sillä vain olemassaoleva yhtäsuuruusoperaatio mahdollistaa avainten vertailun ja tuloksellisen etsintäoperaation.

## Tyyppikonstruktori `Either`

Tyyppi `Either a b` on samankaltainen tyypin `Maybe a` kanssa. 

```haskell
data Either a b = Left a | Right b
```

Tyypillisessä käyttötarkoituksessa operaatio onnistuu palauttaen arvon tyyppiä `Right b` tai epäonnistuu palauttaen virheilmoituksen tyyppiä `Left a`. Vastaavasti lausekkeen jäsentäminen voi sieventää lauseketta `x` arvoon `Right y` tai jättää sen sieventämättä arvolla `Left x`.

Rakennelman kielellinen nerous piilee siinä, että, kuten suomen kielessä, myös englannin kielessä sana *right* tarkoittaa *oikeaa* vastakohtana sekä vasemmalle että väärälle.

Voimme esimerkiksi määritellä funktion `upper`, joka palauttaa tyypin `Maybe Char` arvon riippuen siitä kuuluuko argumentti kirjainjoukkoon `['A'..'Z']` vai ei. Konstruktoriin sovittamalla voimme purkaa argumentin takaisin muuttujaan, kuten esimerkin funktiossa `isIt`.

```haskell
upper x
  | x `elem` ['A'..'Z']  =  Right x
  | otherwise  =  Left x

isIt (Right c) = "Yes: " ++ [c] 
isIt (Left c)  = "No: "  ++ [c] 

> upper 'T'
Right 'T'
> upper 't'
Left 't'
> isIt (upper 'T')
"Yes: T"
> isIt (upper 't')
"No: t"
```

## Kielioppipuut

Kielioppipuut ovat esimerkki rekursiivisista tyypeistä. Kielioppipuun avulla voimme esimerkiksi jäsentää aritmeettisen lausekkeen. Seuraavassa kielioppipuu koostuu yhteenlaskuista (`Add`), kertolaskuista (`Mul`) ja kokonaislukuliteraaleista (`Lit`).

```haskell
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr
```

Määrittelemme tyypin `Expr` kielioppipuun lausekkeelle `(3 + 4) × 7`.

```haskell
tree = Mul (Add (Lit 3) (Lit 4)) (Lit 7)
```

Esitämme saadun kielioppipuun graafisesti kuvassa \ref{fig:expr-tree}.

\begin{figure}[H]
\begin{center}
\includegraphics{expr-tree.pdf}
\caption{Lausekkeen \texttt{(3 + 4) × 7} kielioppipuu.}
\label{fig:expr-tree}
\end{center}
\end{figure}

Kielioppipuun arvon laskemme funktiolla `eval`, jonka määrittelemme seuraavassa:

```haskell
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
```

Saamme

```haskell
> eval tree
49
```

Oksista (*branch*) ja lehdistä (*leaf*) koostuva puu ei suuresti eroa edellisestä.

```haskell
data Tree a = Empty | Leaf a | Branch (Tree a) (Tree a)
```


