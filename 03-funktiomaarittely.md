# Funktion määrittely ja kutsumuodot

## Funktion määrittely

Haskell-ohjelma koostuu funktiomäärittelyistä, joita kutsumme funktion *kutsumuodon* avulla.

Haskell-kielessä määrittelemme funktion yhtälönä, jossa yhtäsuuruusmerkin vasemmalla puolella ovat funktionimi ja parametrit sekä oikealla puolella funktion palautusarvo. Aakkosnumeeriset funktionimet ja parametrit kirjoitamme pienellä alkukirjaimella. 

Esimerkiksi funktiomäärittelyssä

```haskell
double x = 2 * x
```

`double` on funktionimi, `x` on funktion parametri ja `2 * x` on lauseke funktion palautusarvon laskemiseksi.

Kun kutsumme funktiota `double` argumentilla 3, saamme funktion palautusarvoksi 6.

```
> double 3
6
```

Yksinkertaisin muoto funktiomäärittelystä on vakiofunktio, jossa funktiolla ei ole lainkaan parametreja. Määrittelemme seuraavassa vakiofunktiot `i`, `s` ja `ls`.

```haskell
i = 2
s = "Serenitatis"
ls = [1,2,3,4]
```

Funktion kutsumuoto vakiofunktion tapauksessa on yksinkertaisesti funktion nimi ilman argumentteja.

```haskell
> i = 2
> i
2
```

Parametreja sisältävistä funktioista määrittelemme esimerkkinä yhden parametrin funktion `f` ja kahden parametrin funktion `g` standardikirjaston kerto- ja yhteenlaskufunktioiden `(*)` ja `(+)` avulla.

```haskell
f x = x * x
g x y = x * x + y
```

Voimme jakaa funktiomäärittelyn pienempiin osiin esimerkiksi avainsanalla `where` (*missä*).

```haskell
f = a + b
  where
    a = c + 2
    b = c + 4
    c = 3
```

Toinen vaihtoehto funktiomäärittelyn jäsentelyyn on `let` ... `in` (*olkoon* ... *-ssa*) -rakenne.

```haskell
f = let
    a = c + 2
    b = c + 4
    c = 3
  in a + b
```

## Parametrien näkyvyysalueet

Määrittelemme muuttujat `x`, `f`, `g`, `h`, `k`, `p`, `i` ja `j`. Niiden näkyvyysalue on tiedoston päätaso. Ne näkyvät kaikkialla tiedostossa, emmekä voi päätasolla määritellä niitä uudelleen.

```haskell
x = 3
f = x
g x = x + 1
h = x + g x
k = let
    x = 5
    g = 10
  in x + g
p x = x + x
  where
    x = 5
i = g 1
j = p 2
```

Tässä vakiofunktiolla `f` ei ole parametreja. Se saa arvonsa päätason määrittelystä `x = 3`. Muuttujan `f` arvoksi tulee siten 3.

Funktion `g` parametri `x` näkyy ainoastaan funktiomäärittelyn oikealla puolella. Siellä se peittää päätason määrittelyn `x = 3`. Funktio `g` saa siten arvon `x + 1` riippuen siitä mikä on funktion `g` parametri `x`.

Funktiolla `h` ei ole parametreja. Funktion määrittelyssä esiintyvä muuttuja `x` on siksi päätason määrittely `x = 3`. Funktio kutsuu funktiota `g` päätason arvolla `x = 3` eli kutsumuodossa `g 3`, joka saa arvon $3 + 1 = 4$. Muuttuja `h` saa siten arvon $3 + 4 = 7$.

Funktio `k` sisältää `let` ... `in` -rakenteen. Sen sisällä määritellyt muuttujat näkyvät funktiomäärittelyn oikealla puolella, mutta eivät näy päätasolla. Nämä määrittelyt peittävät päätason muuttujat. Muuttuja `k` saa siten arvon $5 + 10 = 15$.

Funktio `p` sisältää `where`-rakenteen. Siinä määrittely `x = 5` peittää sekä parametrin `x` että päätason määrittelyn `x = 3`. Muuttuja `p` saa siten arvon $5 + 5 = 10$.

Funktio `i` kutsuu funktiota `g` arvolla 1, ja saa siten arvon 2. 

Funktio `j` kutsuu funktiota `p` arvolla 2, mutta koska funktion `p` sisäinen määrittely peittää parametrin, ei funktion `p` argumentilla ole vaikutusta tulokseen. Funktion `i` arvo on siten $5 + 5 = 10$.

Muuttujien `f`, `h`, `i`, `j`, `k` ja `x` arvot ovat nyt tietueeksi koottuna 

```haskell
> (f,h,i,j,k,x)
(3,7,2,10,15,3)
```

## Argumenttien sovitus parametreihin

Haskell-ohjelma koostuu päätasolla funktiomäärittelyistä, joissa sidomme joukon muuttujanimiä määrittelyihinsä. Kutsuimme edellä vakiofunktioita niiden nimillä ilman argumentteja. Haskell-kääntäjä etsii tällöin kutsumuotoa eli funktion nimeä vastaavan määrittelyn ja sitoo funktiokutsun tähän määrittelyyn. Funktion määrittely voi sijaita missä tahansa tunnetussa nimiavaruudessa. Määrittelyn tulee kuitenkin olla yksikäsitteinen. Tämän perusteella ymmärrämme, että Haskell-kielessä sama muuttuja- ja funktionimi voi olla määritelty vain yhteen kertaan. Haskell-kielen muuttujille on siten tyypillistä, että ne eivät voi muuttua. 

Vakiofunktiota tavanomaisempia ovat funktiokutsut, joissa annamme funktiolle argumentteja. Esimerkiksi funktiokutsut `f 3` ja `g 4 5` ovat tällaisia. 

Tarkkaan ottaen jokaisella Haskell-funktiolla voi olla korkeintaan yksi parametri. Ymmärrämme tämän myöhemmin parametrien ketjutuksen yhteydessä, jolloin toteamme muun muassa, että lauseke `g x y` suluttuu laskujärjestysmäärittelyiden perusteella muotoon `((g x) y)`. Tarkastelemme tässä kuitenkin lauseketta `g x y` kahden parametrin funktiona.

Haskell-kielelle tyypillinen ominaisuus on, että määrittelemme funktiot sellaisissa parametrimuodoissa, joissa parametrimuoto määrää funktion käyttäytymisen. Parametrimuoto toimii tällöin muottina, johon Haskell-ohjelma sovittaa funktiokutsun argumentteja. Ymmärrämme tässä *muuttujanimiin sovittamisena* ilmiön, jossa funktion kutsumuodosta sovitamme sekä funktion nimen, argumenttien muodon että argumenttien arvon vastaaviin parametreihin määrittelyssä. 

Toisin kuin funktionimiin, joiden tulee olla yksikäsitteisiä, parametreihin sovittaminen tapahtuu lähdekoodissa esitellyssä järjestyksessä, eli ainoastaan lähdekoodissa ylinnä sijaitseva sopiva parametrimuoto tulee hyväksytyksi. Mikäli mikään parametrimuodoista ei tule hyväksytyksi, seurauksena on virhetilanne.

Määrittelemme seuraavassa funktion `head` parametrimuodolla `x:xs`.

```haskell
> head (x:xs) = x
```

Kun nyt kutsumme funktiota argumentilla `[]`, ei argumentti sovi parametrimuotoon `x:xs`.

```haskell
> head []
"ERROR: Non-exhaustive patterns in function `head`."
```

Yksinkertainen tapaus funktion argumentin sovittamisesta on argumentin sovittaminen vakioarvoon.

```haskell
f 1 = "one"
f 2 = "two"
f 3 = "three"
f x = "other: " ++ show x
```
Tässä parametri `x` on vapaa muuttuja, johon kaikki arvot sopivat. Jos funktion `f` argumentti ei siis ole mikään luvuista 1, 2 tai 3, se sopii vapaaseen muuttujaan `x`.

Kuvaamalla nyt funktion `f` listan `[1..6]` alkioille saamme

```haskell
> map f [1..6]
["one","two","three","other: 4","other: 5","other: 6"]
```

## Luetellut tyypit

Määrittelemme *algebralliset tyypit* avainsanalla `data`. Luettelemme määrittelyssä tyypin mahdolliset arvot, josta johtuen kutsumme tällaisia tyyppejä usein *luetelluiksi tyypeiksi*. Esimerkiksi tyyppi `Bool` on lueteltu tyyppi, joka voi saada toisen arvoista `False` tai `True`.

```haskell
data Bool = False | True
```

Argumentin sovituksen avulla voimme määritellä tietotyypille `Bool` funktion `show` parametrien arvoilla `True` ja `False`.

```haskell
show True  = "True"
show False = "False"
```

Saamme

```haskell
> map show [True,False]
["True","False"]
```

## Sovitus konstruktoriin ja purku muuttujiin

Olkoon tyyppi `Shape` lueteltu tyyppi, jonka alkioita ovat ympyrät `Circle r` ja suorakulmiot `Rectangle a b` (kuva \ref{fig:circle-rect}). 

```haskell
data Shape = Circle Float | Rectangle Float Float
```

\begin{figure}[H]
\begin{center}
\includegraphics{circle-rect.pdf}
\caption{Ympyrä \texttt{Circle r} ja suorakulmio \texttt{Rectangle a b}.}
\label{fig:circle-rect}
\end{center}
\end{figure}

Konstruktori `Circle` on yhden parametrin funktio, konstruktori `Rectangle` kahden. Molemmat palauttavat arvon, jonka tyyppi on `Shape`.

```haskell
> :t Circle
Circle :: Float -> Shape
> :t Rectangle
Rectangle :: Float -> Float -> Shape
```

Määrittelemme funktion `area`, joka laskee ympyrän ja suorakulmion pinta-alan

```haskell
area (Circle r) = pi * r * r
area (Rectangle a b) = a * b
```

Funktio saa parametrinaan arvon tyyppiä `Shape` ja palauttaa liukuluvun tyyppiä `Float`. 

```haskell
> :t area
area :: Shape -> Float
```

Voimme määritellä tyyppiä `Shape` olevan ympyrän `c` ja suorakulmion `d`.

```haskell
c = Circle 10
d = Rectangle 2 5
```

Funktio `area` purkaa nyt tyypin `Shape` arvon parametrimuotoon `Circle r` tai `Rectangle a b` riippuen siitä kumpaan muotoon muuttujat `c` ja `d` sopivat. Muuttuja `c = Circle 10` sopii parametrimuotoon `Circle r`, joten vapaa muuttuja `r` saa arvon 10, ja saamme funktion arvon lausekkeella `pi * r * r`. Vastaavasti muuttuja `d = Rectangle 2 5` sopii parametrimuotoon `Rectangle a b`, jolloin saamme funktion arvon lausekkeella `a * b`. Tällöin vapaa muuttuja `a` saa arvon 2, vapaa muuttuja `b` arvon 5, ja lauseke `a * b` arvon $2 \times 5 = 10$.

```haskell
> area c
314.15927
> area d
10.0
```

## Listan purku listakonstruktorin avulla

Määrittelemme listoille funktion `f`, joka purkaa listakonstruktorifunktion `(:)` avulla listan muuttujiin `x` ja `y` samaan tapaan kuin edellä funktio `area` purki tyypin `Shape` arvon parametrimuotoihin `Circle r` ja `Rectangle a b`.

```haskell
> f ((:) x y) = 
  "head = " ++ show x ++ ", " ++ 
  "tail = " ++ show y
> putStrLn (f "Marginis")
head = 'M', tail = "arginis"
```

Listakonstruktori `(:)` esiintyy hyvin yleisesti Haskell-ohjelmissa  parametrimuotoon sovituksen yhteydessä. Esitämme seuraavassa määrittelemämme funktion infix-muodossa.

```haskell
> f (x:y) = 
  "head = " ++ show x ++  ", " ++ 
  "tail = " ++ show y
> putStrLn (f "Humorum")
head = 'H', tail = "umorum"
```

Määrittelemme aiemmin esittelemämme listafunktiot `head` ja `tail` parametrimuotoon sovituksen avulla.

```haskell
head :: [a] -> a
head (x:xs) = x

tail :: [a] -> [a]
tail (x:xs) = xs
```

Halutessamme voimme merkitä käyttämätöntä vapaata muuttujaa alaviivalla `_`, joka on muuttujamäärittelyn erikoismerkki huomiotta jätettävälle muuttujalle.

```haskell
head (x:_)  = x
tail (_:xs) = xs
```

Kaikki epätyhjät listat ovat muotoa `(x:xs)`. Funktiokutsussa `f "Humorum"` on `f` funktionimi ja argumentti `"Humorum"` merkkijono, eli muotoa `(x:xs)` oleva lista.

```haskell
> (:) 'H' "umorum"
"Humorum"
```

Näin ollen argumentti sopii parametrimuodon arvokonstruktoriin `(:)`, jolloin lista `"Humorum"` puretaan vapaisiin muuttujiin `x` ja `xs`. 

Laskujärjestyksessä funktiototeutus on etusijalla verrattuna operaattoriin `(:)`. Kutsumuodon määrittelyssä `f (x:xs)` tarvitsemme sulkumerkkejä laskujärjestyksen osoittamiseen, sillä muutoin funktio `f` sitoisi ensimmäisen muuttujan `x` ja koko lauseke saisi muodon `((f x):xs)`. 

## Tietueen purku tietuekonstruktorin avulla

Jaoimme aiemmin listan `ts` kahtia funktion `splitAt` avulla.

```haskell
> ts = [2,3,5,7,11]
> splitAt 2 ts
([2,3],[5,7,11])
```

Tietuekonstruktorin `(,)` avulla voimme purkaa syntyneen listaparin `p` muuttujiin `a` ja `b`.

```haskell
> p = splitAt 2 ts
> (a,b) = p
> a
[2,3]
> b
[5,7,11]
```

Sama toimii myös prefix-muodossa.

```haskell
> (,) a b = splitAt 2 ts
> a
[2,3]
> b
[5,7,11]
```

## Ehtolauseet parametrin sovituksessa

Sen lisäksi, että voimme pyrkiä sovittamaan argumenttia vakioarvoon tai arvokonstruktoriin, voimme sovittaa sitä ehtolauseeseen.

```haskell
planet name d
  | d < 3.0   = name ++ ": small planet"
  | otherwise = name ++ ": giant planet"
```

Esimerkki tulostaa 

```haskell
> planet "Earth" 1.0
Earth: small planet
> planet "Saturn" 9.4
Saturn: giant planet
```

## Funktiot `putStr` ja `putStrLn`

Funktiokutsu `putStr s` tulostaa merkkijonon `s` standarditulosvirtaan `stdout`. Funktio `putStrLn` tulostaa merkkijonon sekä rivinvaihdon. Päätteeltä käynnistettäessä standarditulosvirtaan `stdout` tulostaminen tarkoittaa tekstin tulostamista pääteikkunaan.

Syöte- ja tulostusfunktioiden tyypiksi on Haskell-kielessä määritelty `IO a`. Tyyppi `IO a` voisi ideaalimaailmassa olla määritelty seuraavasti:

```haskell
data IO a = IO (Realworld -> (Realworld, a))
```

Funktiot `putStr` ja `putStrLn` saavat parametrinaan merkkijonon tyyppiä `String`. Ne palauttavat arvon tyyppiä `IO ()`.

```haskell
> :t putStr
putStr :: String -> IO ()
```

Tyypin `()` ainoa alkio on tyhjä arvo `()`. 

```haskell
data () = ()
```

Jos funktioiden `putStr` ja `putStrLn` palautusarvon tyyppiä `IO ()` haluaisi sanallisesti kuvailla, se voisi olla: "tyhjä arvo samalla standarditulosvirtaan tulostaen". 

## Funktio `show`

Funktio `show` palauttaa annetun argumentin tekstuaalisen esityksen, mikäli sellainen on tyyppiluokassa määritelty eli mikäli tyypillä on tyyppiluokan `Show` instanssi.

```haskell
> s = "g 2 3 = " ++ show (g 2 3)
> s
"g 2 3 = 7"
> putStrLn s
g 2 3 = 7
> show 7
"7"
```

Kun tyyppi `a` on tyyppiluokan `Show` jäsen, on funktion `show` tyyppi

```haskell
> :t show
show :: Show a => a -> String
```

Funktiolla `g` ei ole tyyppiluokan `Show` instanssia.

```haskell
> :t g
g :: Num a => a -> a -> a
> show g
"ERROR: No instance for (Show (a0 -> a0 -> a0))."
```

## Funktio `print`

Standardikirjastossa on määriteltynä myös funktio `print`, joka vastaa likimain funktiota `putStrLn`, mutta tulostaa automaattisen merkkijonomuunnoksen avulla jotakuinkin kaiken tulostettavissa olevan. Samalla se kuitenkin lisää esimerkiksi lainausmerkit merkkijonomuuttujiin. Funktio `print` soveltuu siksi välitulosteisiin ohjelman kehitysvaiheessa, kun taas varsinainen tulostus käy paremmin funktiolla `putStrLn`. Havaitsemme Haskell-kielen vuorovaikutteisen tulkin tulosteen vastaavan funktion `print` paluuarvoja.

```haskell
> (g 2 3)
7
> print (g 2 3)
7
> print "Luxuriae"
"Luxuriae"
> putStrLn "Luxuriae"
Luxuriae
```

Funktion `print` parametri on minkä tahansa tyypin `a` arvo, sillä ehdolla että tyyppi `a` on tyyppiluokan `Show` jäsen, eli sille on määritelty tyyppiluokan `Show` instanssi. Funktion `putStrLn` parametri puolestaan on merkkijono tyyppiä `String`.

```haskell
> :t print
print :: Show a => a -> IO ()
> :t putStrLn
putStrLn :: String -> IO ()
```

## Pääohjelman `main`-funktio

Funktio `main` on erityisasemassa oleva funktio, jota järjestelmä kutsuu kun käynnistämme  ohjelman. Tallennamme oheisen ohjelman tiedostoon `main.hs`.

```haskell
f x = x * x
g x y = x * x + y

main = do
  putStrLn ("f 3 = " ++ show (f 3))
  putStrLn ("g 2 3 = " ++ show (g 2 3))
```

Käynnistämme ohjelman komentoriviltä komennolla `runhaskell main.hs`.

```haskell
$ runhaskell main.hs 
f 3 = 9
g 2 3 = 7
```

Vaihtoehtoisesti voimme lukea ohjelmakoodin vuorovaikutteisen tulkin komennolla `:load` ja kutsua funktiota `main`. Vuorovaikutteinen tulkki antaa kirjastollemme automaattisesti nimen `Main`.

```haskell
> :l main.hs
[1 of 1] Compiling Main        ( main.hs, interpreted )
Ok, modules loaded: Main.
> main
f 3 = 3
g 2 3 = 7
```

Vuorovaikutteisen tulkin komennolla `:browse` näemme määrittelemiemme funktioiden tyyppiallekirjoitukset. 

```haskell
> :browse Main
f :: Num a => a -> a
g :: Num a => a -> a -> a
main :: IO ()
```

## Rekursion periaate

Tutustumme seuraavaksi algoritmin muodossa *rekursioon*, eli  ongelmanratkaisumenetelmään, jossa funktion lopullinen arvo saadaan toistuvasti funktiota itseään kutsumalla. Funktionaalisissa kielissä rekursion merkitys on suuri ja kääntäjä osaa usein muuntaa paljon resursseja vaativan rekursiivisen tehtävän vähän resursseja vaativaksi iteratiiviseksi vastaavaksi.

Listakonstruktorin ja argumenttien sovituksen avulla voimme kirjoittaa seuraavan algoritmin:

```haskell
pairs (x:y:zs) = (x,y) : pairs (y:zs)
pairs (y:zs) = []
```

Algoritmi tuottaa peräkkäiset lukuparit annetusta listasta.

```haskell
> print (pairs [1..5])
[(1,2),(2,3),(3,4),(4,5)]
```

Muistamme, että lista `[1..5]` on muotoa `1:2:3:4:5:[]`.

Tässä parametrimuoto `x:y:zs` vastaa ainoastaan sellaisia listoja, joiden alussa on kaksi listan alkiota `x` ja `y` ja näiden perässä lista `zs`. Kutsuttaessa funktiota argumentilla `[1..5]` tämä ehto täyttyy, sillä listan `1:2:3:4:5:[]` kohdalla on voimassa `x:y:zs = 1:2:(3:4:5:[])`. Muuttujien arvot ovat siten `x = 1`, `y = 2` ja `zs = 3:4:5:[]`.

Funktion palautusarvo näillä muuttujan arvoilla on pari `(1,2)` jota seuraa listakonstruktori ja tämän jälkeen uusi funktiokutsu `pairs`, mutta nyt argumentilla `y:zs` eli `2:(3:4:5:[])` joka vastaa listaa `[2..5]`. Tämä funktiokutsu luonnollisesti palauttaa parin `(2,3)`, listakonstruktorin ja jälleen uuden funktiokutsun. 

Jatkamme näin kunnes argumentin muoto ei enää vastaa kutsumuotoa `x:y:zs`. Tällöin algoritmi siirtyy tarkastelemaan seuraavaa kutsumuotoa `y:zs`. Listan `[1..5]` kohdalla näin käy ainoastaan listan lopussa, kun jäljellä ovat enää alkiot `5` ja `[]`. Saamme tällöin funktion toisen kutsumuodon mukaisesti palautusarvona tyhjän listan `[]`, ja voimme lähteä kokoamaan koko funktion palautusarvoa listakonstruktorien avulla. Päädymme muotoon `(1,2):(2,3):(3,4):(4,5):[]` joka vastaa tuloslistaa `[(1,2),(2,3),(3,4),(4,5)]`.

Muistamme, että Haskell-kielessä parametrimuotojen esittelyjärjestyksellä on merkitystä.

Edellisen funktion kutsumuotojen määritteleminen päinvastaisessa järjestyksessä johtaa hyvin erilaiseen tulokseen, sillä kaikki toisen kutsumuodon mukaiset argumentit toteuttavat myös ensimmäisen kutsumuodon, eikä rekursio näin ollen pääse edes alkuun ennen kuin se on jo ohitse.

```haskell
> :{
| pairs' (y:zs) = []
| pairs' (x:y:zs) = (x,y) : pairs' (y:zs)
| :}
> print (pairs' [1..5])
[]
```

## Rekursiivisia funktioita

Määrittelemme nyt itse standardikirjaston funktiot `length`, `sum` ja `product` rekursion avulla.


```haskell
length [] = 0
length (x:xs) = 1 + length xs

sum [] = 0
sum (x:xs) = x + sum xs

product [] = 1
product (x:xs) = x * product xs
```

Funktiot `factorial` ja `(++)` määrittelisimme rekursion avulla seuraavasti:


```haskell
factorial 0 = 1
factorial n = n * factorial (n - 1)

[] ++ ys     = ys
(x:xs) ++ ys = x : xs ++ ys
```

Rekursiiviset funktiot voivat myös olla päättymättömiä.

```haskell
ones = 1 : ones
numsFrom n = n : numsFrom (n + 1)
squares = map (^ 2) (numsFrom 0)
```

Saamme nyt esimerkiksi

```haskell
> take 6 ones 
[1,1,1,1,1,1]
> take 5 (numsFrom 5)
[5,6,7,8,9]
> take 7 squares
[0,1,4,9,16,25,36]
```


