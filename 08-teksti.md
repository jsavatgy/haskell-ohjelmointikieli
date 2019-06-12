# Teksti, sanat ja kirjaimet

## Argumentit komentoriviltä

Kun käynnistämme ohjelman komentotulkissa, voimme antaa sille komentorivillä argumentteja. Argumenttien lukemiseksi tuomme kirjastosta `System.Environment` funktion `getArgs`.

```haskell
import System.Environment (getArgs)
```

Kirjoitamme lyhyen ohjelman `get-arcs.hs`.

```haskell
main = do
  args <- getArgs
  print args
```

Komentoriviltä käynnistettynä ohjelma tulostaa `print`-komennon avulla listan argumenteista.

```
$ runhaskell get-arcs.hs 
[]
$ runhaskell get-arcs.hs arg1 arg2 arg3 arg4
["arg1","arg2","arg3","arg4"]
```

## Tyyppikonstruktori `IO`

Funktion `getArgs` tyyppi on `IO [String]`. Tyyppi `IO a` sisältää tyyppimuuttujan `a`, joka voi olla mikä tahansa tyyppi. Kun tyyppimuuttujan `a` arvo on `a` = `[String]`, saamme tyypin `IO [String].` 

```haskell
> :type getArgs
getArgs :: IO [String]
```

Haskell-kieli vaatii konstruktorin `IO` kaikilta operaatioilta, jotka lukevat syötettä standardisyötevirrasta tai tulostavat standarditulosvirtaan. 

Haskell-kääntäjän interaktiivisen tulkin komennolla `:info` näemme, että tyyppi `IO a` on tyyppiluokkien `Monad`, `Functor` ja `Applicative` jäsen, eli sille on määritelty kunkin tyyppiluokan instanssi. Tyyppi `IO a` on myös tyyppiluokan `Monoid` jäsen sillä ehdolla että `a` on sen jäsen.

```haskell
> :info IO
newtype IO a = 
...
instance Monad IO
instance Functor IO
instance Applicative IO
instance Monoid a => Monoid (IO a)
```

`Do`-lausekkeen avulla voimme koota yhteen toimenpiteitä monadissa. Sijoitettuamme arvon listaan `args`, nimeämme listan ensimmäisen alkion muuttujaksi `fileName`. `Do`-lausekkeessa tämä toimenpide vaatii avainsanan `let`. Lopuksi kutsumme funktiota `analyse` argumentilla `fileName`.

```haskell
main = do
  args <- getArgs
  let fileName = args !! 0
  analyse fileName
```

Sijoitusnuolta `<-` käytämme, kun luemme arvon monadista. Tarvitsemme sijoitusnuolta aina kun funktion palautusarvolla on monadinen konstruktori, kuten tässä tapauksessa `IO`. Sijoitusnuoli `<-` purkaa arvon muuttujaan, joka on nyt tyyppiä `[String]`. 

Määrittelemme seuraavaksi `do`-lausekkeen avulla funktion `analyse`.

```haskell
analyse fileName = do
  content <- readFile fileName
  putStrLn "frequency content = "
  putStrLn (count1 content)
```

Funktio saa parametrin `fileName`, joka on tiedoston nimi merkkijonona. Funktio lukee tiedoston sisällön komennolla `readFile` muuttujaan `content` ja tulostaa kaksi merkkijonoa, joista toinen määräytyy funktiokutsun `count1 content` tuloksena.

Kuten muistamme, saa tulostusfunktio `putStrLn` argumenttinaan merkkijonon, jonka se tulostaa standarditulosvirtaan. Standarditulosvirtaan tulostaminen merkitsee, että funktion `putStrLn` palautusarvo on tyyppiä `IO a`. Kun annamme tyyppimuuttujalle arvon `a = ()` saamme abstraktista tyypistä `IO a` yksinkertaisimman mahdollisen konkreettisen tyypin `IO ()`

```haskell
> :t putStrLn
putStrLn :: String -> IO ()
```

Myös funktion `analyse` palautusarvo on tyyppiä `IO ()`. Funktio koostuu peräkkäisistä lausekkeista, joiden kaikkien palautusarvoilla on konstruktori `IO`.

```haskell
> :t analyse
analyse :: FilePath -> IO ()
```

## Funktiot `readFile`, `writeFile` ja `appendFile`

Funktiokutsu `readFile f` lukee tiedoston `f` ja palauttaa sen sisällön merkkijonona. Funktio saa ensimmäisenä parametrinaan arvon tyyppiä `FilePath`. Interaktiivisen tulkin komennolla `:info` saamme selville, että tyyppi `FilePath` on merkkijonotyypin `String` synonyymi. Nimi `FilePath` toimii tässä dokumentoinnin apuvälineenä. Vaikka tyypin `FilePath` arvo voi olla mikä tahansa merkkijono, kertoo nimi, että järjestelmä odottaa tuon merkkijonon olevan myös mielekäs tiedostopolku.

```haskell
> :t readFile
readFile :: FilePath -> IO String
> :i FilePath
type FilePath = String
```

Funktiokutsu `writeFile f s` kirjoittaa tiedostoon `f` merkkijonon `s`.

```haskell
> str1 = "Oceanus Procellarum\nMare Frigoris\nMare Imbrium\n"
> writeFile "moon1.txt" str1
``` 

Funktiokutsu `appendFile f s` lisää tiedostoon `f` merkkijonon `s`.

```haskell
> str2 = "Mare Fecunditatis\n"
> appendFile "moon1.txt" str2
```

Olemme näin tallentaneet tiedostoon `moon1.txt` tekstirivit, jotka esitämme käyttöjärjestelmän komennolla `cat`.

```haskell
> :!cat moon1.txt
Oceanus Procellarum
Mare Frigoris
Mare Imbrium
Mare Fecunditatis
```

## Funktiot `lines` ja `words`

Luemme tekstin funktiokutsulla `readFile "moon1.txt"` muuttujaan `content`.

```haskell
> content <- readFile "moon1.txt"
> content
"Oceanus Procellarum\nMare Frigoris\nMare Imbrium\nMare
 Fecunditatis\n"
```

Muuttuja `content` sisältää nyt merkkijonon, jossa kaikki tiedoston rivit ovat peräjälkeen rivinvaihtosymbolilla `'\n'` (*newline*) erotettuina. 

Funktio `lines` palauttaa tekstin rivit listana. Funktio `words` palauttaa tekstin sanat listana. 

```haskell
> lines content
["Oceanus Procellarum","Mare Frigoris","Mare Imbrium",
 "Mare Fecunditatis"]
> words content
["Oceanus","Procellarum","Mare","Frigoris","Mare","Imbrium",
 "Mare","Fecunditatis"]
```

Funktio `words` soveltuu myös välilyöntimerkkien siivoamiseen sanojen ympäriltä.

```haskell
> words "  lacus  "
["lacus"]
```

Molemmat funktiot saavat parametreinaan merkkijonon tyyppiä `String` ja palauttavat merkkijonojen listan tyyppiä `[String]`.

```haskell
> :t lines
lines :: String -> [String]
> :t words
words :: String -> [String]
```

Aakkosmerkkien ulkopuolelle jäävät merkit voimme siivota pois myös määrittelemällä funktion `trim`.

```haskell
trim = dropWhileEnd (not . isAlpha) . 
  dropWhile (not . isAlpha)
```

Funktio tarvitsee kirjastosta `Data.Char` funktion `isAlpha` ja kirjastosta `Data.List` funktion `dropWhileEnd`. Funktio `dropWhile` pudottaa alkioita (merkkejä) pois listan (merkkijonon) alusta ja funktio `dropWhileEnd` listan (merkkijonon) lopusta. 

```haskell
> import Data.Char
> import Data.List
> trim = dropWhileEnd (not . isAlpha) . 
|   dropWhile (not . isAlpha)
> trim "34Excellentiae. "
"Excellentiae"
```

Jos haluamme säilyttää aakkosmerkkien lisäksi myös numerot, käytämme funktion `isAlpha` sijasta funktiota `isAlphaNum`.

```haskell
> trim2 = dropWhileEnd (not . isAlphaNum) . 
|   dropWhile (not . isAlphaNum)
> trim2 " \t8.8 W.\n"
"8.8 W"
```

## Funktiot `unwords` ja `unlines` 

Funktiot `unwords` ja `unlines` suorittavat edellä kuvatut operaatiot vastakkaiseen suuntaan (olematta kuitenkaan täydellisiä käänteisfunktioita funktioille `words` ja `lines`).

```haskell
> unwords ["Oceanus","Procellarum","Mare","Frigoris",
    "Mare","Imbrium","Mare","Fecunditatis"]
"Oceanus Procellarum Mare Frigoris Mare Imbrium Mare
 Fecunditatis"
> unlines ["Oceanus Procellarum","Mare Frigoris",
    "Mare Imbrium","Mare Fecunditatis"]
"Oceanus Procellarum\nMare Frigoris\nMare Imbrium\nMare
 Fecunditatis\n"
```

## Kirjainten esiintymistiheyden laskeminen

Haluamme laskea kirjainten esiintymistiheyden Alexandre Dumas'n suomennetusta teoksessa Kolme muskettisoturia. Luemme tekstin funktiolla `readFile`.

```haskell
> content <- readFile "kolme-muskettisoturia.txt"
```

Valitsemme sisällöstä kaikki merkit, jotka ovat kirjaimia. Tätä varten tuomme kirjastosta `Data.Char` funktion `isLetter`. Funktio `isLetter` palauttaa totuusarvon `True` mikäli merkki on Unicode-merkistön aakkosmerkki, muussa tapauksessa se palauttaa totuusarvon `False`.

Tuomme kirjastosta `Data.Char` myös funktion `toUpper`. Funktio `toUpper` muuntaa merkin suuraakkosiksi.

```haskell
> import Data.Char (toUpper,isLetter)
> t = [(toUpper c, 1) | c <- content, isLetter c]
> length t
1039627
> take 10 t
[('K',1),('O',1),('L',1),('M',1),('E',1),('M',1),('U',1),
 ('S',1),('K',1),('E',1)]
```

Muuttuja `t` sisältää nyt vähän yli miljoonan tietueen listan, jossa kukin tietue koostuu kahdesta alkiosta: kirjaimesta suuraakkosin ja luvusta 1.

Muodostamme seuraavaksi hakupuun listasta `t`. Tätä varten tuomme kirjastosta `Data.Map` funktiot `toList` ja `fromListWith`.

```haskell
> import Data.Map (toList,fromListWith)
```

Funktiokutsu `fromListWith f xs` muodostaa hakupuun `Map` listasta `xs`. Funktio saa parametrinaan funktion `f`, joka on kuvaus siitä, mitä tapahtuu, kun samalle avaimelle on jo olemassa aikaisempi arvo. Edellisessä esimerkissä näin tapahtuu ensimmäisen kerran avaimen `'M'` kohdalla, kun olemme löytäneet listasta alkiot `('M',1)` ja `('M',1)`. Kun nyt aikaisempi arvo on `a = 1` ja uusi arvo `b = 1`, haluamme laskea uuden alkuarvon funktiolla `a + b`, joka on prefix-muodossa `(+) a b` ja parametrittomassa muodossa `(+)`.

M-kirjaimen kohdalla sama toistuu valitsemamme tekstin puitteissa 33839 kertaa. Alkion loppuarvoksi tulee siten `('M',33839)`.

```haskell
> fromListWith (+) t
fromList [('A',120742),('B',937),('C',1400),('D',11062),
...
> list1 = (toList . fromListWith (+)) t
> list1
[('A',120742),('B',937),('C',1400),('D',11062),('E',82849),
...
```

Kirjastossa `Data.Map` on myös funktio `fromList`, jolla ei ole parametria `f`. Funktio `fromList` ei huomioi päällekkäisiä arvoja, vaan viimeksi tullut arvo korvaa aikaisemmin tulleen. Näemme funktioiden `fromList` ja `fromListWith` tyypit vuorovaikutteisen tulkin komennolla `:type`. Huomaamme myös, että yhteenlaskuoperaation `(+)` tyyppi on parametrilta `f` vaadittavaa tyyppiä.

```haskell
> :t fromList
fromList :: Ord k => [(k, a)] -> Map k a
> :t fromListWith
fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> Map k a
> :t (+)
(+) :: Num a => a -> a -> a
> :t fromListWith (+)
fromListWith (+) :: (Num a, Ord k) => [(k, a)] -> Map k a
```

Osittaistoteutettu funktiokutsu `fromListWith (+)` kokoaa tyyppiehdot tyyppimuuttujille `k` ja `a`. Avaimen tyypin `k` tulee olla järjestyvä (*Orderable*) ja arvon tyypin `a` numeerinen (*Numeric*). Tyyppiehdot ovat siten `(Num a, Ord k)`. Nyt siis, sillä ehdolla että tyyppi `k` on järjestyvä ja tyyppi `a` numeerinen, saa funktiokutsu `fromListWith (+)` parametrinaan listan tyyppiä `[(k, a)]` ja palauttaa hakupuun tyyppiä `Map k a`.

Hakupuun tekstuaalinen esitys (tyyppiluokan `Show` instanssi) on määritelty muotoon `fromList [...]`. Voimme esittää tekstin perusteella syntyneen hakupuun avaimet myös graafisesti (kuva \ref{fig:musket-tree}). 

Hakupuun toiminta on yksinkertainen. Vertaamme etsittävää avainta juureen. Jos avain on järjestyksessä ennen juurta, siirrymme vasempaan oksaan. Jos avain on järjestyksessä juuren jälkeen, siirrymme oikeaan oksaan. Toistamme operaation kunnes avain löytyy tai saavutamme oksan kärjen avainta löytämättä.

\begin{figure}[ht]
\begin{center}
\includegraphics{musket-tree.pdf}
\caption{Kirjastorutiinin \texttt{fromListWith} muodostama hakupuu.}
\label{fig:musket-tree}
\end{center}
\end{figure}

Voimme etsiä avaimeen liitetyn arvon myös funktiolla `(!)`.

```haskell
> map1 = fromListWith (+) t
> map1 ! 'M'
33839
```

## Pylväsdiagrammin piirtäminen

Laadimme seuraavaksi ohjelman, joka piirtää pylväsdiagrammin kirjainten esiintymistiheydestä. Tarvitsemme muun muassa kirjastot `Data.Map`, `Data.List` ja `Data.Char`.

```haskell
import Data.Map (toList,fromListWith,(!))
import Data.List (sortOn)
import Data.Char (toUpper,isLetter)
```

Luemme esimerkkitekstin tiedostosta. Koska joidenkin kirjainten esiintymistiheys on varsin vähäinen, päätämme huomioida ainoastaan 24 yleisintä kirjainta.

```haskell
fileName = "kolme-muskettisoturia.txt"

count fileName = do
  content <- readFile fileName
  return (fiProb (frequencyC content))

main = do
  c <- count fileName 
  putStrLn (tpict (take 24 c))
```

Laskemme kirjainten esiintymistiheyden aiemman esimerkin mukaisesti.

```haskell
frequencyC content = (reverse . sortOn snd) result
  where
    result = (toList . fromListWith (+)) t
    t = [(toLower c, 1) | c <- content, isLetter c]

sortBySnd =  reverse . sortOn snd
sumOfSnd fr = sum [n | (c,n) <- fr]

fiProb cnt =
  [([c],intToDouble n / intToDouble sm) | (c,n) <- fr]
  where
    sm = sumOfSnd fr
    fr = cnt
```

Piirrämme pylväät määrittelemällä funktion `blocks1`. Funktion ainoa parametri `prob` on lista merkkien esiintymistiheyksistä. Funktiossa `block` piirrämme pylvästä kuvaavan viivajonon, joka alkaa nollatasolta, kulkee pylvään kahden huippupisteen kautta ja päättyy takaisin nollatasolle.

Nollatason viiva `baseline` on pylväille yhteinen, ja piirrämme sen siksi erikseen.

```haskell
block c x1 x2 y1 y2 = 
  [Texttt (Point xm y2) c "above"] ++
  [rct] 
  where
    rct = PolyLine [p1,p2,p3,p4]
    p1 = Point x1 y1 
    p2 = Point x1 y2
    p3 = Point x2 y2
    p4 = Point x2 y1 
    xm = (x1+x2) / 2
    ym = (y1+y2) / 2

columns y yts = concat rs ++ [baseline]
  where 
    baseline = Line (Point (x1 1 - baseExtra*eps) 0) 
      (Point (x2 (length yts) + baseExtra*eps) 0)
    rs = [block c (x1 xk) (x2 xk) 0 (bscale * y2) 
      | (xk,(c,y2)) <- zip [1..] yts]
    x1 xk = intToDouble xk * eps - (bsize*eps)
    x2 xk = intToDouble xk * eps + (bsize*eps)
    baseExtra = 0.15
    bsize = 0.40
    bscale = 3.00
    eps = 1.00 / (n + 1)
    n = intToDouble (length yts)

blocks1 prob = 
  columns 0.0 prob
```  

Piirrämme mittakaavajanan kuvion vasemmalle puolelle määrittelemällä funktion `scale1`. Funktion ensimmäinen viivajono sisältää mittakaavajanan pystyviivan ja uloimmat sakarat, seuraava listamuodostimella muodostettu viivojen lista sisemmät sakarat ja viimeinen tekstien lista jakovälit numeroina.

Olemme esittäneet valmiin kuvion kuvassa \ref{fig:histogram}.

\begin{figure}[ht]
\begin{center}
\includegraphics[width=\textwidth]{fi-histogram-01.pdf}
\caption{Yleisimmät 24 kirjainta esiintymistiheyksineen Alexandre Dumas's suomennetussa teoksessa Kolme muskettisoturia.}
\label{fig:histogram}
\end{center}
\end{figure}

```haskell
scale x y dxs dys = [ PolyLine [ 
    sPoint (x + minimum dxs) (y + maximum dys),
    sPoint (x + maximum dxs) (y + maximum dys), 
    sPoint (x + maximum dxs) (y + minimum dys), 
    sPoint (x + minimum dxs) (y + minimum dys)] ] ++
  [Line 
    (sPoint (x + minimum dxs) (y + d)) 
    (sPoint (x + maximum dxs) (y + d)) 
    | d <- (tail . init) dys] ++
  [Texttt (sPoint x (y + dy))
    (show2 dy) "left" | dy <- dys]
  where
    sPoint x1 y1 = Point x1 (bscale * y1)
    bscale = 3.00

scale1 = 
  scale (-0.01) 0.0 [0,0.01] [0.00,0.03 .. 0.12]
```

Edellä kuvattuja menetelmiä käyttäen saamme myös kirjainjonon, jossa otoksessa esiintyvät kirjaimet ovat esiintymistiheyden mukaisessa järjestyksessä.

```haskell
> content <- readFile "kolme-muskettisoturia.txt"
> putStrLn [c | (c,n) <- frequencyC content]
AINTESÄLOKUMHRVYJPDGÖCBXÉFWQZÈÊÂÆ
```

Sanojen esiintymistiheyden laskemiseen määrittelemme funktion `frequencyW`.

```haskell
frequencyW content = (size1,size2,result)
  where 
    size2 = length result
    size1 = length t
    result = (toList . fromListWith (+)) t
    t = [(w, 1) | w <- trimmed, not (null w)]
    trimmed = (map trim) ws
    ws = words c1
    c1 = (map toLower) content
```

Tässä muuttuja `size1` palauttaa sanojen yhteismäärän ja muuttuja `size2` eri sanojen määrän. 

Jos haluamme muotoilla numerot kolmen ryhmiksi, voimme määritellä funktion `show3g`.

```haskell
show3g n = reverse ic
  where
    ic = intercalate " " (chunksOf 3 r1)
    r1 = reverse (show n)
```

Perinteisen aakkosjärjestyksen sijasta voimme käyttää myös aakkosjärjestyksenä kirjainten esiintymistiheyttä.

\par\penalty-800

```haskell
let charCnt = countC content 
    result1 = sortByContent fr1 charCnt

countC content = [c | (c,n) <- frequencyC content]

sortByContent fr charCnt = sortOn fun fr
  where
    elmIndex x = 
      elemIndex (toLower x) charCnt
    fun (x,y) = [Just ((-1) * y)] ++ map elmIndex x
```

Määrittelemällä funktion `printStats` muotoilemme tulosteen sanojen yhteismäärästä.

```haskell
printStats size1 size2 = do
  putStrLn ("\nYhteensä **" ++ show3g size1 ++ 
    "** sanaa, joista **" ++ show3g size2 ++
    "** erilaista.\n")
```

Tulokseksi saamme seuraavan taulukon:


