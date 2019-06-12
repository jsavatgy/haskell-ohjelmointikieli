# Listat

Haskell-kielen listan käsite abstrahoi yksisuuntaisen linkitetyn listan toteutuksen alkioineen, linkkeineen ja loppumerkkeineen (kuva \ref{fig:linked-list}).

\begin{figure}[H]
\begin{center}
\includegraphics{linked-list.pdf}
\caption{Lista \texttt{xs = [3,5,7,11]} yksisuuntaisena linkitettynä listana.}
\label{fig:linked-list}
\end{center}
\end{figure}

Haskell-kielessä lista on yksinkertainen mutta monikäyttöinen tietorakenne.

Kuten kuvasta \ref{fig:linked-list} havaitsemme, tapahtuu listan käsittely alkaen ensimmäisestä alkiosta, ja listan viimeisen alkion saavutamme vasta käytyämme listan kaikki muut alkiot lävitse.

Kun käsittelemme listoja, emme muuta yksittäisten alkioiden sisältöä, vaan kopioimme olemassaolevia listoja tai listan osia, joita vaihdamme, liitämme ja poistamme haluamamme tuloksen saavuttamiseksi.

## Listan määrittely

Määrittelemme listat muodollisesti monimuotoisena rekursiivisena tietotyyppinä. Lista on joko tyhjä lista `[]` tai minkä tahansa tyypin `a` alkio liitettynä listakonstruktorilla vastaavan tyypin listaan. 

```haskell
data [a] = [] | a : [a]
```

Äärelliset listat rakentuvat siten alkaen tyhjästä listasta `[]`. Listat ovat oikealle assosioivia eli ne kasvavat oikealta vasemmalle. Listan pää sijaitsee vasemmalla ja listan häntä oikealla. 

Listan häntä on lista, kun taas listan pää on alkio.

```haskell
> xs = [3,5,7,11]
> head xs
3
> tail xs
[5,7,11]
> head (tail xs)
5
> tail (tail xs)
[7,11]
```

Lista `[]` on tyhjä lista, lista `[5]` yhden alkion muodostama lista ja luku `5` listan alkio. Listakonstruktorifunktiolla `(:)` emme voi liittää toisiinsa kahta listaa, vaan ensimmäisen argumentin tulee olla listan alkio.

```haskell
> [5] : [11]
"ERROR: Non type-variable argument in the constraint."
```

Listat voivat olla myös listojen muodostamia listoja. Listojen muodostamien listojen alkiot ovat listoja.

```haskell
> [5] : [[11]]
[[5],[11]]
```

## Listakonstruktorifunktio `(:)`

Operaattori `(:)` on listakonstruktorifunktio ja kutsumme sitä englanninkielisellä nimellä *cons*. Listakonstruktorifunktion avulla liitämme alkion listan alkuun.

```haskell
> 3:(5:(7:(11:[])))
[3,5,7,11]
> 3:5:7:11:[]
[3,5,7,11]
> :info (:)
data [] a = ... | a : [a] 	-- Defined in ‘GHC.Types’
infixr 5 :
```

Listakonstruktorifunktio `(:)` vaatii ensimmäisenä argumenttinaan alkion ja toisena argumenttinaan listan.

```haskell
> 3 : [5,7]
[3,5,7]
> 5 : [11]
[5,11]
```

Listakonstruktorifunktion `(:)` tyyppi on `a -> [a] -> [a]`.


```haskell
> :type (:)
(:) :: a -> [a] -> [a]
```

## Funktio `(++)`

Funktio `(++)` saa argumentteinaan kaksi listaa. Se palauttaa listan, jossa argumentteina annetut listat on yhdistetty. Funktion `(++)` tyyppi on `[a] -> [a] -> [a]`.

```haskell
> [2,3] ++ [5,7,11]
[2,3,5,7,11]
> [5] ++ [11]
[5,11]
> :type (++)
(++) :: [a] -> [a] -> [a]
```

Funktiot `(:)` ja `(++)` toimivat myös prefix-muodossa. 

```haskell
> (:) 1 []
[1]
> (:) 2 [3,4]
[2,3,4]
> (++) [3,4] [5,6]
[3,4,5,6]
```


## Merkkijonot listoina

Haskell-kielessä merkkijonot ovat merkkien muodostamia listoja. 

```haskell
type String = [Char]
```

Yksittäisen merkin (tyyppiä `Char`) kirjoitamme yksinkertaisiin lainausmerkkeihin (`'F'`) ja merkkijonon (tyyppiä `String`) tuplalainausmerkkeihin (`"Frigoris"`). Kaksi merkkijonoa liitämme yhteen listat yhdistävällä operaattorilla `(++)`.

```haskell
> :t 'F'
'F' :: Char
> :t "Frigoris"
"Frigoris" :: [Char]
> ['M','e','d','i','i'] 
"Medii"
> 'S':'m':'y':"thii"
"Smythii"
> "Imbr" ++ "ium"
"Imbrium"
```

Kuten muidenkin listojen kohdalla, funktio `(++)` vaatii argumentteinaan kaksi listaa ja funktio `(:)` alkion ja listan.

```haskell
> 'M' ++ "ortis"
"ERROR: Couldn't match type ‘[Char]’ with ‘Char’."
> 'O':'d':'i':'i'
"ERROR: Couldn't match type ‘[Char]’ with ‘Char’."
```

## Sovittaminen parametrimuotoon

Esimakua Haskell-kielelle ominaisesta ilmiöstä, argumentin sovittamisesta parametrimuotoon, saamme, kun listakonstruktorioperaattorin `(:)` avulla puramme listan kahdeksi muuttujaksi, pääksi ja hännäksi.

```haskell
> x:xs = [1,2,3,4]
> x
1
> xs
[2,3,4]
```

## Listamuodostimet

Nimitämme *listamuodostimeksi* listarakennetta, joka sisältää kuvauksen listan alkioiden ulkomuodosta, alkioiden tuottamiseen käytettävän algoritmin ja mahdollisesti säännöt tiettyjen alkioiden suodattamiseen saadusta tuloslistasta.

Alkioiden `x` muodostaman listan, jossa `x` on joukosta `[1..5]`, saamme listamuodostimella

```haskell
> [x | x <- [1..5]]
[1,2,3,4,5]
```

Nimitämme lausetta `x <- [1..5]` listamuodostimen *generaattoriksi*. 

Sellaisten alkioiden muodostaman listan, jossa alkiot ovat muotoa `[x]`, saamme listamuodostimella

```haskell
> [[x] | x <- [1..5]]
[[1],[2],[3],[4],[5]]
```

Peräkkäisten lukujen muodostamien lukuparien muodostaman listan saamme listamuodostimella

```haskell
> [(x,x+1) | x <- [1..5]]
[(1,2),(2,3),(3,4),(4,5),(5,6)]
```

Muotoa `Just x` olevien alkioden listan saamme listamuodostimella

```haskell
> [Just x | x <- [1..5]]
[Just 1,Just 2,Just 3,Just 4,Just 5]
```

Listamuodostin voi myös sisältää *suotimia*, jotka rajoittavat tulosjoukkoa. Seuraavassa funktio `odd` toimii listamuodostimen suotimena rajoittaen tulosjoukon parittomiin lukuihin.

```haskell
> [x | x <- [1..10], odd x]
[1,3,5,7,9]
```

Sekä generaattoreita että suotimia voi olla useita. Generaattoreista jäljempi toimii silmukkana edeltävän sisällä.

```haskell
> [(x,y) | x <- [1..3], y <- [1..2]]
[(1,1),(1,2),(2,1),(2,2),(3,1),(3,2)]
```

Kaikki `(x,y)`-parit, joissa `x` on pariton ja joukosta `[1..5]` sekä `y` parillinen ja joukosta `[4..8]` saamme listamuodostimella

```haskell
> [(x,y) | x <- [1..5], y <- [4..8], odd x, even y]
[(1,4),(1,6),(1,8),(3,4),(3,6),(3,8),(5,4),(5,6),(5,8)]
```

## Funktio `concat`

Standardikirjaston funktio `concat` muuntaa listojen listan listaksi.

```haskell
> concat [[1,2],[3,4],[5]]
[1,2,3,4,5]
> concat ["Ma","re Ma","rgin","is"]
"Mare Marginis"
```

Listojen lisäksi funktio `concat` toimii myös muille tyyppiluokan `Foldable` jäsenille. Listojen tapauksessa tyyppimuuttuja `t` saisi arvon `t` = `[]` ja funktio `concat` tyyppiallekirjoituksen `concat :: [[a]] -> [a]`.

```haskell
> :t concat
concat :: Foldable t => t [a] -> [a]
```

## Funktio `concatMap`

Funktiokutsu `concatMap f xs` kuvaa funktion `f` listalle `xs` muuntaen tuloksena saadun alilistojen listan peräkkäiseksi listaksi. Funktio `concatMap` palauttaa näin ollen yhdellä kertaa sen mitä funktiot `map` ja `concat` peräjälkeen kutsuttuina palauttaisivat.

```haskell
> replicate 3 [1,2]
[[1,2],[1,2],[1,2]]
> map (replicate 3) [1,2]
[[1,1,1],[2,2,2]]
> concatMap (replicate 3) [1,2]
[1,1,1,2,2,2]
```

Voimme esimerkiksi määritellä funktiokutsun `head1 xs`, joka palauttaa listan `xs` pään yhden alkion listana silloin, kun lista `xs` ei ole tyhjä, ja tyhjän listan silloin, kun `xs` on tyhjä lista. Nyt funktiokutsu `concatMap head1 xs` kuvaa funktion `head1` listalle `xs` ja yhdistää syntyneet alilistat.

```haskell
> head1 xs = if (null xs) then [] else [head xs]
> map head1 ["Lacus","","Odii"]
["L","","O"]
> concatMap head1 ["Lacus","","Odii"]
"LO"
```

Funktion `concatMap` tyyppi on `(a -> [b]) -> t a -> [b]`, sillä ehdolla, että `t` kuuluu tyyppiluokkaan `Foldable`. Esimerkiksi listat ovat tyyppiluokan `Foldable` jäsen.

```haskell
> :t concatMap
concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
```


