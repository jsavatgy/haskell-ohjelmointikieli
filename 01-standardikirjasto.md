
# Haskell-ohjelmointikieli

Haskell-kieli on nykyaikainen funktionaalinen ohjelmointikieli. Kielen ensimmäinen versio julkaistiin vuonna 1990. Haskell-ohjelmat voidaan joko kääntää konekielelle tai ajaa tulkattuina. Käännetyt ohjelmat toimivat itsenäisesti ja ovat hyvin tehokkaita. Tulkkaus puolestaan mahdollistaa vuorovaikutteisuuden kielen kanssa. Tavallisesti modernit tietokoneohjelmat sisältävät sekä käännettyjä että tulkattavia ominaisuuksia. 

## Vuorovaikutteinen tulkki `ghci` ja kääntäjä `ghc`

Aloitamme tutustumisen Haskell-kieleen käynnistämällä Haskell-kääntäjän (*Glasgow Haskell Compiler*) interaktiivisen tulkin komennolla 

```haskell
ghci
```

Kun myöhemmin käännämme ohjelmia lähdetiedostoista, käytämme Haskell-kääntäjää komennolla `ghc`. Esimerkiksi ohjelman lähdetiedostosta `example.hs` kääntäisimme komennolla 

```haskell
ghc example.hs
```

Kokeiluvaiheessa voimme käynnistää ohjelman (esimerkiksi `example.hs`) lähdetiedostosta Haskell-tulkin suoritettavaksi ilman käännösvaihetta komennolla 

```haskell
runhaskell example.hs
```

Käynnistettäessä Haskell-kääntäjän tulkki kertoo ohjelman version ja jää odottamaan käyttäjän komentoja. 

```haskell
$ ghci
GHCi, version 8.0.2: http://www.haskell.org/ghc/  
> 
```

Voimme käyttää Haskell-tulkkia vaikkapa laskimena.

```haskell
> 64 * 16
1024
```

Haskell-kielen standardikirjasto `Prelude` tarjoaa oletuksena käytettäväksemme joukon funktioita ja tietorakenteita, joihin tutustumme tässä ensimmäisessä luvussa.  


## Prefix- ja infix-funktiomuodot

Haskell-kielessä funktiot esiintyvät *prefix-* ja *infix-*muodoissa.

Nimitämme prefix-muotoiseksi funktiota, jossa funktionimi edeltää parametreja. Funktiota, jossa funktionimi on parametrien välissä, nimitämme infix-muotoiseksi. 

Haskell-kielessä kirjoitamme aakkosnumeeriset funktionimet prefix-muodossa sellaisenaan, infix-muodossa ympäröimme ne niin sanotuilla yksinkertaisilla takalainausmerkeillä `` `f` `` (englanniksi *backtick*, *grave accent*, ASCII-koodi 96). 

Operaattorit (eli muut kuin aakkosnumeeriset funktionimet) ympäröimme prefix-muodossa kaarisulkeilla `()`, infix-muodossa kirjoitamme ne sellaisenaan. 

Esimerkiksi Haskell-kielen infix-muotoisen lausekkeen `2 + 3` voimme esittää prefix-muodossa seuraavasti:

```haskell
> (+) 2 3
5
```  

Tietueiden `(x,y)` ja listojen `[x,y]` esitystapaa Haskell-kielessä nimitämme *mixfix*-muodoksi.

## Funktiot `min` ja `max`

Funktio `min` palauttaa kahdesta annetusta argumentista pienemmän ja funktio `max` suuremman. Kutsumme näitä funktioita seuraavassa prefix-muodossa.

```haskell
> min 2 5
2
> max 5 7
7
```

Voimme kirjoittaa saman myös infix-muodossa

```haskell
> 2 `min` 5
2
> 5 `max` 7
7
```

## Funktiot `abs`, `even` ja `odd`

Funktiokutsu `abs` $x$ palauttaa luvun $x$ itseisarvon $|x|$. 

```haskell
> abs (-3)
3
```

Funktiokutsu `even` $x$ palauttaa totuusarvon `True` luvun $x$ ollessa parillinen, muutoin se palauttaa arvon `False`. Funktiokutsu `odd` $x$ palauttaa totuusarvon `True` luvun $x$ ollessa pariton, muutoin se palauttaa arvon `False`.

```haskell
> odd 2
False
> even 4
True
```

## Operaattorit `(+)`, `(-)`, `(*)` ja `(/)`

Peruslaskutoimitukset on Haskell-kielessä toteutettu funktioina `(+)`, `(-)`, `(*)` ja `(/)`. Käytämme näitä operaattoreita yleisemmin infix-muodossa. 

```haskell
> 3 - 4
-1
> 4 * 5
20
> 1 / 2
0.5
```

Desimaalilukujen desimaalierottimena toimii piste ja merkitsemme edeltävän nollan näkyviin matemaattisia merkintätapoja seuraten. Negatiiviset luvut ympäröimme kaarisulkeilla `()`, jolloin kääntäjä erottaa ne vähennyslaskuoperaatiosta. 

```haskell
> 5 * (-0.02)
-0.1
> (-1) + (-2)
-3
```

## Funktiot `div`, `mod` ja `divMod`

Funktio `div` palauttaa kokonaislukujen jakolaskun kokonaisosan ja funktio `mod` jakojäännöksen. 

```haskell
> 7 `div` 3
2
> 7 `mod` 3
1
```

Funktio `divMod` palauttaa kahden alkion tietueena jakolaskun kokonaisosan ja jakojäännöksen. 

```haskell
> 7 `divMod` 3
(2,1)
```

## Operaattorit `(==)`, `(/=)`, `(<)`, `(>)`, `(<=)` ja `(>=)` 

Funktiot `(==)`, `(/=)`, `(<)`, `(>)`, `(<=)` ja `(>=)` määrittävät matemaattiset vertailuoperaatiot $=$, $\neq$, $<$, $>$, $\leq$ ja $\geq$. Ne kukin palauttavat totuusarvon `True` tai `False`.

```haskell
> 5 == 5
True
> 4 /= 5
True
> 5 < 2
False
```

Muun muassa palautusarvosta ja määrittämättömästä assosiatiivisuudesta johtuen emme voi kirjoittaa useampia loogisia vertailuoperaatioita ketjuun.

```haskell
> 1 < 2 < 5
"ERROR: Precedence parsing error."
```

Sen sijaan jaamme lausekkeen operaatiot pienempiin osiin. 

```haskell
> (1 < 2) && (2 < 5)
True
```

## Funktiot `not`, `(&&)` ja `(||)` 

Funktio `not` on yhden parametrin totuusarvofunktio. Se vastaa logiikan operaatiota **ei** ($\neg$) eli se palauttaa negaation annetusta argumentista. 

Funktiot `(&&)` ja `(||)` ovat kahden parametrin funktioita. Ne vastaavat logiikan operaatioita **ja** ($\land$) ja **tai** ($\lor$). Käytämme funktioita `(&&)` ja `(||)` yleensä infix-muodossa. 

Funktio `(&&)` palauttaa totuusarvon `True`, mikäli molemmat argumentit ovat arvoltaan `True`, muutoin se palauttaa arvon `False`. Funktio `(||)` palauttaa totuusarvon `True`, mikäli vähintään toinen argumentti on arvoltaan `True`, muutoin se palauttaa arvon `False`.

```haskell
> not False
True
> not (even 3)
True
> True && False
False
> True || False
True
```

## Muuttujien määrittely

Määrittelemme seuraavassa kokonaislukumuuttujat `a = 2`, `b = 3` ja `c = a + b`. Nyt muuttujan `c` arvo on `c` = `a + b` = 2 + 3 = 5. 

```haskell
> a = 2
> b = 3
> c = a + b
> c
5
```

Vastaavasti määrittelemme totuusarvomuuttujat `p = True` ja `r = False`. Nyt lausekkeen `p || r` arvo on `True || False` = `True`.

```haskell
> p = True
> r = False
> p || r
True
```

Seuraavassa määrittelemme merkkimuuttujat `c1 = 'e'` ja  `c2 = 'o'`. Lausekkeen `c1 < c2` arvo on nyt `'e' < 'o'` = `True`. (Aakkosjärjestys määrää merkkien suuruusjärjestyksen.)

```haskell
> c1 = 'e'
> c2 = 'o'
> c1 < c2
True
```

Kun määrittelemme funktion `plus = (+)`, saamme aiemmilla muuttujien `a = 2` ja `b = 3` arvoilla lausekkeen `` a `plus` b `` arvoksi `a + b` = 2 + 3 = 5.

```haskell
> plus = (+)
> a `plus` b
5
```

Määrittelemällä funktion `(×) = (*)` saamme lausekkeen `3 × 4` arvoksi 12.

```haskell
> (×) = (*)
> 3 × 4
12
```

Seuraavassa merkkijonomuuttuja `s` saa arvon `s = "Mare Australe"` ja funktio `size` arvon `size = length`. Standardikirjastossa määritelty funktio `length` palauttaa merkkijonon (listan) pituuden, joten myös funktio `size` palauttaa merkkijonon (listan) pituuden. Lausekkeen `size s` arvo on siten 13.


```haskell 
> s = "Mare Australe"
> size = length
> size s
13
```

## Tiedon esittäminen listoina

Nimitämme *listaksi* samaa tyyppiä olevien alkioiden jonoa. Listan merkintätapa Haskell-kielessä on ympäröivät hakasulkeet `[]`. 

Esimerkkinä määrittelemme listan `ts`, jonka alkioita ovat luvut 2, 3, 5, 7 ja 11.

```haskell
> ts = [2,3,5,7,11]
> ts
[2,3,5,7,11]
```

Nimeämme listat usein englannin kielen monikkomuodon mukaisesti päätteellä -`s` (`ts`, `xs`, ...).

Lista `[]` on tyhjä lista, lista `[5]` yhden alkion lista, lista `[2,3]` kahden alkion lista, ja niin edelleen.

## Funktiot `head` ja `tail`
 
Funktio `head` palauttaa listan ensimmäisen alkion, jota nimitämme listan *pääksi*. Funktio `tail` palauttaa ensimmäistä alkiota lukuun ottamatta loput listan alkiot, eli listan *hännän*. 

```haskell
> head ts
2
> tail ts
[3,5,7,11]
```

Havaitsemme, että funktiot `head` ja `tail` eivät sovellu tyhjien listojen käsittelyyn. Pyrimme siksi välttämään näiden funktioiden käyttöä kaikissa sellaisissa tapauksissa, joissa argumentti saattaa olla tyhjä lista.

```haskell
> head []
"ERROR: Empty list."
> tail []
"ERROR: Empty list."
```

## Funktiot `and` ja `or`

Funktiot `and` ja `or` ovat kahden parametrin funktioiden `(&&)` ja `(||)` vastineet listojen käsittelyyn.

Funktiokutsu `and` $s$ palauttaa totuusarvon `True`, mikäli listan $s$ kaikki alkiot ovat arvoltaan `True`, muutoin se palauttaa arvon `False`. 

Funktiokutsu `or` $s$ palauttaa totuusarvon `True`, mikäli yksikin listan $s$ alkioista on arvoltaan `True`, muutoin se palauttaa arvon `False`.

```haskell
> and [True,False,True]
False
> or [True,False,True]
True
```

## Funktiot `any` ja `all`

Funktiokutsu `any` $p$ $s$ palauttaa totuusarvon `True`, mikäli jokin listan $s$ alkioista täyttää ehdon $p$, muutoin se palauttaa arvon `False`. Funktiokutsu `all` $p$ $s$ palauttaa arvon `True`, mikäli kaikki listan $s$ alkiot täyttävät ehdon $p$, muutoin se palauttaa arvon `False`.

```haskell
> ts = [2,3,5,7,11]
> any (even) ts
True
> all (odd) ts
False
> any (> 10) ts
True
> all (< 10) ts
False
```

## Funktiot `sum` ja `product`

Funktiot `sum` ja `product` palauttavat numeerisen listan alkioiden summan ja tulon. 

```haskell
> sum ts
28
> product ts
2310
```

## Funktiot `minimum` ja `maximum`

Funktiot `minimum` ja `maximum` palauttavat järjestyvän listan pienimmän ja suurimman alkion. 

```haskell
> minimum ts
2
> maximum ts
11
```

## Funktiot `take`, `drop` ja `(!!)`

Funktiokutsu `take` $n$ $s$ palauttaa $n$ ensimmäistä alkiota listasta $s$. Funktiokutsu `drop` $n$ $s$ pudottaa pois $n$ ensimmäistä alkiota listasta $s$. 

```haskell
> ts = [2,3,5,7,11]
> take 3 ts
[2,3,5]
> drop 2 ts
[5,7,11]
```

Funktio `(!!)` palauttaa listasta alkion, jolla on argumentin mukainen järjestysluku. Listan alkioiden numerointi alkaa luvusta 0. 

```haskell
> ts !! 3
7
```

## Funktiot `elem` ja `NotElem`

Funktio `elem` palauttaa totuusarvon `True` mikäli annettu alkio on listan jäsen, muutoin se palauttaa arvon `False`. Funktio `notElem` palauttaa totuusarvon `True` mikäli annettu alkio ei ole listan jäsen, muutoin se palauttaa arvon `False`. 

```haskell
> 7 `elem` ts
True
> 1 `elem` ts
False
> 1 `notElem` ts
True
```

## Funktiot `last` ja `init`

Funktio `last` palauttaa listan viimeisen alkion ja funktio `init` listan alun aina toiseksi viimeiseen alkioon saakka. 

```haskell
> ts = [2,3,5,7,11]
> last ts
11
> init ts
[2,3,5,7]
```

## Funktio `reverse`

Funktio `reverse` palauttaa käänteisen listan.

```haskell
> reverse ts
[11,7,5,3,2]
```

## Funktiot `length` ja `null`

Funktio `length` palauttaa listan alkioiden lukumäärän. Funktio `null` palauttaa totuusarvon `True`, mikäli lista on tyhjä, muutoin se palauttaa arvon `False`.

```haskell
> ts = [2,3,5,7,11]
> length ts
5
> null ts
False
> null []
True
```

## Kuvausfunktio `map`

Funktiokutsu `map` $f$ $s$ kuvaa funktion $f$ listalle $s$. 

```haskell
> ts = [2,3,5,7,11]
> map (* 2) ts
[4,6,10,14,22]
> map (+ 11) ts
[13,14,16,18,22]
> map negate ts
[-2,-3,-5,-7,-11]
```

## Funktio `filter`

Funktiokutsu `filter` $p$ $s$ suodattaa listan $s$ ja jättää jäljelle ehdon $p$ täyttävät alkiot.

```haskell
> filter (< 10) ts
[2,3,5,7]
> filter even ts
[2]
> filter odd ts
[3,5,7,11]
```

## Funktio `splitAt`

Funktiokutsu `splitAt` $n$ $s$ jakaa listan $s$ kahden alkion tietueeksi annetusta katkaisukohdasta $n$. Alkioiden numerointi alkaa luvusta 0. 

```haskell
> ts = [2,3,5,7,11]
> splitAt 2 ts
([2,3],[5,7,11])
```

## Tietueet

Nimitämme *tietueeksi* alkioiden jonoa, jossa alkiot eivät välttämättä ole toistensa kanssa samaa tyyppiä. Tietueen merkintätapa Haskell-kielessä on ympäröivät kaarisulkeet `()`. Esimerkkinä määrittelemme kahden alkion lukuparin $p = (2,3)$.
 
```haskell
> p = (2,3)
> p
(2,3)
```

Lukupari on esimerkki vektorimuotoisesta tiedon esittämisestä. Merkintämuoto on laajennettavissa useampiulotteisiin vektoreihin, esimerkiksi `q = (2,7,5)` tai `r = (2,3,6,7)`. 

```haskell
> q = (2,7,5)
> q
(2,7,5)
> r = (2,3,6,7)
> r
(2,3,6,7)
```

Tietueen voimme määritellä myös prefix-muodossa.

```haskell
> (,) 1 2
(1,2)
> (,,) 3 4 7
(3,4,7)
```

Ilmaisemme yksiulotteiset vektorit skalaareina. Kääntäjä tulkitsee mahdolliset sulkumerkit tällöin lausekkeen arvoon ja laskuoperaatioihin kuuluviksi.

```haskell
> (5)
5
```

Tietueen alkiot voivat edustaa eri tyyppejä. Esimerkiksi kokonaisluvun ja merkin muodostamat parit ovat sallittuja. Samaan tapaan tietueet voivat edelleen koostua alitietueista.

```haskell
s = (1,'a')
t = (1,2,'b')
u = ((1,'a'),(1,2,'b'),1)
```
## Funktiot `fst` ja `snd`

Funktiokutsu `fst` $(a,b)$ palauttaa kahden alkion tietueesta $(a,b)$ ensimmäisen alkion $a$. Funktiokutsu `snd` $(a,b)$ palauttaa tietueen $(a,b)$ toisen alkion $b$.

```haskell
> p = (2,3)
> fst p
2
> snd p
3
```

Funktioiden `fst` ja `snd` määrittelyt ovat seuraavan kaltaiset:

```haskell
fst (a,b) = a
snd (a,b) = b
```

## Lukujonot listoina

Haskell-kielessä voimme määritellä listat aritmeettisina lukujonoina antamalla jonon ensimmäisen ja viimeisen alkion. Oletuksena lista kasvaa yhden yksikön verran jokaista alkiota kohden. 

Voimme myös antaa lukujonon ensimmäisen, toisen ja viimeisen alkion $x_1$, $x_2$ ja $x_k$ muodossa `[`$x_1$, $x_2$`..`$x_k$`]`, jolloin ohjelma laskee puuttuvat alkiot kaavalla $$x_n = x_{(n-1)} + (x_2 - x_1)$$

```haskell
> [3..7]
[3,4,5,6,7]
> [6,5..1]
[6,5,4,3,2,1]
> [1,3..11]
[1,3,5,7,9,11]
```

Vastaavalla tavalla voimme muodostaa myös muiden järjestyvien joukkojen, kuten reaalilukujen tai merkkien, muodostamia listoja.

```haskell
> [1.5..5.5]
[1.5,2.5,3.5,4.5,5.5]
> ['a'..'g']
"abcdefg"
```

## Päättymättömät listat

Edellä kuvattua menetelmää käyttäen voimme muodostaa päättymättömiä listoja. 

```haskell
xs = [1..]
zs = [1,3..]
```

Tässä lista `xs` sisältää kaikki kokonaisluvut alkaen luvusta 1. Lista `zs` sisältää kaikki parittomat luvut alkaen luvusta 1.

## Funktiot `takeWhile` ja `dropWhile`

Funktiokutsu `takeWhile` $p$ $s$ palauttaa listan $s$ alkioita alusta niin pitkälle kuin ehto $p$ on voimassa. Funktiokutsu `dropWhile` $p$ $s$ pudottaa listan $s$ alkioita pois alusta niin pitkälle kuin ehto $p$ on voimassa.

```haskell
> takeWhile (< 10) [1,3..]
[1,3,5,7,9]
> dropWhile (< 3) [1,2,3,4,5,1,2,3]
[3,4,5,1,2,3]
```

Päättymättömiä listoja emme voi suodattaa funktiolla `filter`. Esimerkiksi funktiokutsu `filter (< 10) [1..]` palauttaa yhdeksän ensimmäistä kokonaislukua ja jatkaa lukujen seulontaa tuloksetta kunnes käyttäjä sen keskeyttää. 

```haskell
> filter (< 10) [1..]
[1,2,3,4,5,6,7,8,9 CTRL-C Interrupted.
```

Funktion `filter` sijasta käytämmekin funktiota `takeWhile`. Funktiokutsun `filter (< 10) [1..]` esitämme siksi muodossa `takeWhile (< 10) [1..]`.

```haskell
> takeWhile (< 10) [1..]
[1,2,3,4,5,6,7,8,9]
```

## Funktiot `repeat` ja `cycle`

Funktio `repeat` tuottaa päättymättömän listan, jossa annettu alkio toistuu. Funktio `cycle` tuottaa päättymättömän listan, jossa annetun listan alkiot toistuvat.

```haskell
> repeat 1
[1,1,1,...]
> repeat [1,2]
[[1,2],[1,2],[1,2],...]
> cycle [1,2]
[1,2,1,2,1,2,...]
```

Funktiot `take` ja `head` toimivat päättymättömille listoille samaan tapaan kuin äärellisille listoille. Ne palauttavat äärellisen määrän alkioita ja jättävät päättymättömän listan muut alkiot huomiotta.

```haskell
> take 5 [1..]
[1,2,3,4,5]
> head [1..]
1
> take 10 (repeat 1)
[1,1,1,1,1,1,1,1,1,1]
> take 5 (repeat [1,2])
[[1,2],[1,2],[1,2],[1,2],[1,2]]
> take 10 (cycle [1,2])
[1,2,1,2,1,2,1,2,1,2]
```

## Funktio `zip`

Funktio `zip` muodostaa pareja yhdistämällä alkioita kahdesta listasta alkio kerrallaan. Funktio palauttaa uuden listan, kun lyhyempi alkuperäisistä listoista on läpikäyty.

```haskell
> ts = [2,3,5,7,11]
> vs = [1,5,10,14]
> zip ts vs
[(2,1),(3,5),(5,10),(7,14)]
```

Voimme luonnollisesti yhdistää listan itsensä tai oman häntänsä kanssa. Esimerkiksi lista `tail ts` on listan `ts` häntä. Nyt lauseke `tail ts` saa arvon `tail [2,3,5,7,11]` = `[3,5,7,11]` ja lauseke `zip ts (tail ts)` arvon `[(2,3),(3,5),(5,7),(7,11)]`.

```haskell
> zip ts ts
[(2,2),(3,3),(5,5),(7,7),(11,11)]
> zip ts (tail ts)
[(2,3),(3,5),(5,7),(7,11)]
```

Kuten funktiot `take` ja `head`, myös funktio `zip` soveltuu päättymättömien listojen käsittelyyn.

```haskell
> zip [0..] ts
[(0,2),(1,3),(2,5),(3,7),(4,11)]
> zip [1..] ['a'..'g']
[(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e'),(6,'f'),(7,'g')]
```

## Funktio `zipWith`

Funktio `zipWith` on funktion `zip` yleistetty versio. Funktiokutsun `zipWith` $f$ $s_1$ $s_2$ parametri $f$ on funktio, joka kertoo säännön kuinka tulosalkio muodostetaan, kun lähtöalkiot saadaan annetuista listoista $s_1$ ja $s_2$. 

```haskell
> zipWith (+) [1..7] [1..]
[2,4,6,8,10,12,14]
> zipWith (*) [1..7] [1..]
[1,4,9,16,25,36,49]
> zipWith (,) [1..] ['a'..'g']
[(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e'),(6,'f'),(7,'g')]
```

Kun funktion `zipWith` ensimmäinen argumentti on tietuekonstruktorifunktio `(,)`, saamme yhtäpitävän määrittelyn edellä esittelemällemme funktiolle `zip`.

```haskell
> zip' = zipWith (,)
> zip' [1..] ['a'..'g']
[(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e'),(6,'f'),(7,'g')]
```

## Tyyppijärjestelmä

Haskell-kieli on vahvasti tyypitetty kieli. Tämä tarkoittaa, että kaikilla arvoilla on tyyppi. Mikäli emme erikseen määritä arvon tyyppiä, kääntäjä päättelee tyypin. Kun arvojen tyypit ovat käännöshetkellä tiedossa, kääntyy ohjelma konekielelle tehokkaammin.

Tavanomaisia tietotyyppejä ovat kokonaislukutyyppi `Int`, liukulukutyypit `Float` ja `Double`, merkkityyppi `Char` sekä totuusarvotyyppi `Bool`.

```haskell
i :: Int
i = 5
f :: Float
f = 1.5707964
d :: Double
d = 6.283185307179586
c :: Char
c = 'e'
b :: Bool
b = True
```

Tyyppimäärittelyn `x :: y` luemme: "muuttuja $x$ on tyyppiä $y$". Esimerkiksi edellä määrittelemämme muuttuja `c` on tyyppiä `Char`.

## Funktiotyyppi

Matematiikasta meille ovat tuttuja kokonaislukujen joukon symboli $\mathbb{Z}$, ja funktion määrittelyjoukon merkintä, missä esimerkiksi funktio $f$ kuvaa kokonaisluvun kokonaisluvuksi

$$f: \mathbb{Z} \to \mathbb{Z}$$

Edellä luetelluista Haskell-kielen tyypeistä vastaa matematiikan kokonaislukuja lähinnä tyyppi `Int`. Funktion kuvausnuolena toimii merkkipari `->`. Funktion, joka kuvaa tyypin `Int` luvun tyypin `Int` luvuksi, tyyppiallekirjoitus on siten

```haskell
f :: Int -> Int
```

## Abstraktit tietotyypit

Vahvasta tyypityksestä huolimatta Haskell-kielessä funktiot voivat operoida alkioilla, joiden tyyppiä emme ole erikseen määränneet. Näin on esimerkiksi silloin, kun alkioiden tyypillä ei ole funktion tuloksen kannalta merkitystä. Toisaalta ohjelman suunnittelun tasolla voimme toisinaan säilyttää tyypin käsitteen abstraktiona, joka saa käytännön toteutuksensa vasta ohjelman yksityiskohtien tarkentuessa. Sanomme tällaista tyyppiä *abstraktiksi* tyypiksi. Kuvaamme abstraktia tietotyyppiä tyyppimuuttujalla, jonka kirjoitamme pienellä alkukirjaimella, esimerkiksi `t`. Tyyppi `t` voi olla mikä tahansa tyyppi.

Abstraktit tyypit voivat esiintyä esimerkiksi funktioiden tyyppiallekirjoituksissa.
Tällöin yhden parametrin funktio `f x`, joka palauttaa muuttujan `x` kanssa samaa tyyppiä olevan arvon, on tyyppiä `a -> a`.

```haskell
f :: a -> a
```

Yhden parametrin funktio `g x`, jonka palautusarvo ei ole riippuvainen muuttujan `x` tyypistä, on tyyppiä `a -> b`.

```haskell
g :: a -> b
```

Kahden parametrin funktio `h x y` on tyyppiä `a -> b -> c` ja kolmen parametrin funktio `k x y z` tyyppiä `a -> b -> c -> d`.

```haskell
h :: a -> b -> c
k :: a -> b -> c -> d
```

## Tyyppiluokat

Voimme asettaa abstrakteille tyypeille lisävaatimuksen kuulua tiettyihin tyyppiluokkiin. Vaatimuksena tyypille voi olla esimerkiksi (suluissa tyyppiluokka) mahdollisuus verrata alkioiden välistä yhtäsuuruutta (`Eq`), järjestyvyysominaisuus (`Ord`), lueteltavuus (`Enum`), numeerisuus (`Num`), desimaalimuotoisuus (`Floating`), osamäärämuotoisuus (`Fractional`), mahdollisuus esittää arvo merkkijonona (`Show`) tai mahdollisuus lukea arvo merkkijonosta (`Read`).

Esitämme tyypille asetettavan lisävaatimuksen seuraavasti:

```haskell
(+) :: Num a => a -> a -> a
min :: Ord a => a -> a -> a
(/) :: Fractional a => a -> a -> a
sin :: Floating a => a -> a
show :: Show a => a -> String
elem :: (Eq a, Foldable t) => a -> t a -> Bool
```

Tässä esimerkiksi funktion `(+)` parametrit voivat olla mitä tahansa tyyppiä `a` sillä ehdolla, että tyyppi `a` on numeerinen eli tyyppiluokan `Num` jäsen.

## Luetellut tyypit

Luetellut eli algebralliset tyypit määrittelemme avainsanalla `data`. Annamme tyypin nimen yhtäsuuruusmerkin vasemmalla puolella ja tyypin alkioiden mahdolliset arvot (*konstruktorit*) oikealla puolella. Esimerkiksi seuraavassa tietotyyppi `Bool` voi saada toisen arvoista `False` (*epätosi*) tai `True` (*tosi*), ja tietotyyppi `Direction` (*ilmansuunta*) jonkin arvoista `N`, `S`, `E`, `W`, `NE`, `NW`, `SE` tai `SW`.

```haskell
data Bool = False | True
data Direction = N | S | E | W | NE | NW | SE | SW 
```

## Korkeamman asteen tyypit

Korkeamman asteen tyypeillä on konstruktori ja parametreja. 

```haskell
data Point = Point Double Double
data RGB = RGB Double Double Double 
```

Tässä tyypin `Point` arvoilla on konstruktori `Point` ja kaksi liukulukutyypin `Double` argumenttia. Tyypin `RGB` arvoilla on konstruktori `RGB` ja kolme tyypin `Double` argumenttia. Tyypillä on usein sama nimi kuin sen ensimmäisellä konstruktorilla. 

## Konstruktorit funktioina

Kohtelemme konstruktoreita Haskell-kielessä funktioina, joilla on tyyppi. Konstruktorien ainoa tehtävä on muodostaa alkio, joka on annettua tyyppiä. Konstruktoreiden toteutus on siten niiden määrittelyssä, joten emme Haskell-kielessä luo taikka hävitä konstruktoreita erikseen. Seuraavassa esitämme konstruktorien `False` ja `Point` tyypit.

```haskell
False :: Bool
Point :: Double -> Double -> Point
```

## Kommentit lähdekoodissa

Haskell-kielessä voimme kirjoittaa ohjelmakoodiin kommentteja tavuviivaparin `--` jälkeen. Kääntäjä jättää tällöin kommenttimerkit ja sitä seuraavan tekstin huomiotta rivin loppuun saakka. Useamman rivin kommentit voimme ympäröidä merkkipareilla `{-` ja `-}`. Kommenttimerkintöjen avulla voimme myös piilottaa varsinaista ohjelmakoodia kääntäjältä näkymättömiin.

Haskell-kielen kääntäjäympäristöön kuuluu dokumentointiohjelma Haddock. Sen avulla voimme luoda automaattisesti ohjesivun kirjoittamallemme kirjastolle. Funktion ja tietotyypin määrittelyn yläpuolella oleva kommenttimerkintä 

```haskell
-- | Explanation what a function does
```

on ohje dokumentointiohjelmalle, jonka perusteella se yhdistää kommentin sitä vastaavaan määritelmään.

## Merkitsevä sisennys

Kuten perustellusti monessa muussa nykyaikaisessa ohjelmointikielessä, myös Haskell-kielessä ohjelmakoodin sisennys on merkityksellinen ja siten pakollinen. Sisennys jäsentää muuttujamäärittelyjen näkyvyyden eri tasoille. Esimerkiksi `where`-rakenteen sisällä määritellyt muuttujat eivät näy päätasolla.

```haskell
s = a + b
  where
    a = 5
    b = 7
```

Hyvä sisennysmäärä on esimerkiksi kaksi välilyöntiä. Sisennykseen ei pidä käyttää tabulaattoria.

Emme käy sisennyssääntöjä tässä tarkemmin lävitse, sillä ne noudattavat useimmin samaa päättelylogiikkaa ihmisen kanssa. 

Toisinaan saatamme ohjeista huolimatta kirjoittaa määritelmiä esimerkiksi vuorovaikutteisessa tulkissa yhdelle riville käyttäen aaltosulkeita `{}` ja puolipistettä `;`.

```haskell
s = a + b where { a = 5; b = 7 }
```

## Vuorovaikutteisen tulkin komentoja

Tässä luvussa esittelimme joitakin standardikirjaston funktioita, joiden toimintaa kokeilimme Haskell-kielen vuorovaikutteisessa tulkissa. Vuorovaikutteinen tulkki `ghci` tarjoaa myös joukon omia komentoja, jotka eivät ole Haskell-kielen funktioita. Vuorovaikutteisen tulkin omat komennot alkavat kaksoispisteellä `:`. 

Vuorovaikutteisen tulkin komentoluettelon saamme komennolla `:help`. Kuten useimmat komennot, se lyhenee alkukirjaimen mukaan muotoon `:h` tai `:?`.

Jos esimerkkiohjelmamme on tiedostossa `example.hs`, lataamme sen vuorovaikutteiseen tulkkiin komennolla `:load example`.

Tehtyämme muutoksia tiedostoon, lataamme tiedoston uudelleen komennolla `:reload`.

Komento `:browse` tulostaa kirjaston määrittelemät funktiot ja tietorakenteet. Esimerkiksi kattavan luettelon standardikirjaston `Prelude` funktioista saamme komennolla `:browse Prelude`.

Komennolla `:type` näemme annetun lausekkeen tyypin.

```haskell
> :type 'c'
'c' :: Char
> :type (+)
(+) :: Num a => a -> a -> a
```

Komento `:info` tulostaa tietoa annetusta funktiosta tai tietorakenteesta.

```haskell
> :info max
class Eq a => Ord a where
  max :: a -> a -> a
...
> :info Int
data Int = GHC.Types.I# GHC.Prim.Int# 	
                                 -- Defined in ‘GHC.Types’
instance Bounded Int              -- Defined in ‘GHC.Enum’
instance Enum Int                 -- Defined in ‘GHC.Enum’
...
```

Muuttuja `it` palauttaa kulloinkin viimeksi lasketun lausekkeen arvon.

```haskell
> 3 * 4
12
> it + 1
13
```

Komennolla `:!` suoritamme komentotulkin komennon.

```haskell
> :! date
pe 4.1.2019 21.16.43 +0200
```

Useammalle riville jaetut määrittelyt voimme antaa komentoparin `:{` ja `:}` sisällä.

```haskell
> :{
| s = a + b
|  where
|   a = 5
|   b = 7
| :}
> s
12
```

Olemme tämän kirjan tulosteessa käyttäneet komentokehotteena merkkiparia `"> "` ja useammalle riville jaettujen määrittelyiden kehotteena merkkiparia `"| "`. Käytetyt komentokehotteet voimme asettaa komennoilla `:set prompt` ja `:set prompt-cont`. Kun lisäämme komennot tiedostoon `~/.ghc/ghci.conf`, tulkki suorittaa ne automaattisesti käynnistyksen yhteydessä. Voimme asettaa komentokehotteille myös värimäärityksiä.

```haskell
:set prompt  "\ESC[34m\STX> \ESC[m\STX"
:set prompt-cont "\ESC[34m\STX| \ESC[m\STX"
```

Vuorovaikutteisesta tulkista poistumme komennolla `:quit`.


