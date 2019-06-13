
# Pallogeometriaa

## Kuun mitat ja pinnanmuodot

Kuun keskimääräinen säde on 1737.1 kilometriä. Suurimmat pinnanmuodot ovat varhaisia törmäyskraattereita. Kappaleen törmätessä kuun pintaan sula basalttinen laava täytti kraatterin muodostaen tumman tasaisen alangon, joita nykyisin nimitämme kuun meriksi (*maria*), järviksi (*lacus*), lahdiksi (*sinus*) ja soiksi (*paludes*). 

Oletamme, että käytössämme on kuvan \ref{fig:moon-coordlist} mukainen listaus kuun merkittävimmistä pinnanmuodoista tekstitiedostona. Kukin rivi koostuu tabulaattorimerkein erotetuista kentistä. Kentät ovat muodostuman latinankielinen nimi, suomenkielinen nimi, latitudi, longitudi ja halkaisija.

\begin{figure}[!htbp]
\begin{center}
\includegraphics{moon-coordlist.pdf}
\caption{Kuun merkittävimmät pinnanmuodot tekstitiedostona. Kentät on erotettu tabulaattorimerkein.}
\label{fig:moon-coordlist}
\end{center}
\end{figure}

## Funktio `splitOn`

Kun annamme muuttujan `str` arvoksi esimerkkirivin tiedostosta, voimme jakaa merkkijonon osiin kirjaston `Data.List.Split` funktiolla `splitOn`. Funktio saa argumentteinaan katkaisevan ja katkaistavan merkkijonon. Funktio palauttaa listan syntyneistä merkkijonon osista.

```haskell
> import Data.List.Split
> str = "Mare Smythii\tSmythin meri\t1.3 N\t87.5 E\t373"
> splitOn "\t" str
["Mare Smythii","Smythin meri","1.3 N","87.5 E","373"]
```

## Pallokoordinaatisto

Voimme muuntaa koordinaatteja pallokoordinaatistosta karteesiseen koordinaatistoon kaavalla
(<http://mathworld.wolfram.com/SphericalCoordinates.html>)
$$\begin{aligned}
  x&=r \cos \theta \sin \phi \\
  y&=r \sin \theta \sin \phi \\
  z&=r \cos \phi 
\end{aligned}$$

Tässä $r$ on säde eli etäisyys origosta, $\theta$ kulma $x$-akselista $xy$-tasossa ja $\phi$ kulma ylöspäin osoittavasta $z$-akselista. 

Maantieteellisessä koordinaatistossa merkitsemme leveysastetta (*latitudi*) symbolilla $\delta$, jolloin $\phi = 90^{\circ} - \delta$ sekä pituusastetta (*longitudi*) symbolilla $\lambda$ ($\lambda = \theta$).

Koordinaattilyhenteissä kirjain N (*north*) merkitsee pohjoista leveyttä, S (*south*) eteläistä leveyttä, E (*east*) itäistä pituutta ja W (*west*) läntistä pituutta. Maapallolla leveysaste $\delta$ kasvaa päiväntasaajalta pohjoiseen kuljettaessa ja pituusaste $\lambda$ Greenwichin nollameridiaanilta itään kuljettaessa. Nimitykset leveys ja pituus juontuvat Välimeren alueen kulttuureista: Välimeri on "pitkä" itä-länsi-suunnassa ja "leveä" pohjois-etelä-suunnassa. Pituuspiirejä sanotaan myös meridiaaneiksi. Termi meridiaani johtuu latinan puolipäivää tai etelää merkitsevästä sanasta *meridies*.

Määrittelemme tietotyypin `Point3D` pisteelle kolmiulotteisessa karteesisessa $xyz$-koordinaatistossa. Pallokoordinaatistossa määrittelemme pisteen `Spheric3D` kulmien $\theta$ ja $\phi$ avulla.

```haskell
-- | Point3D x y z, RH cartesian coordinates
data Point3D = Point3D Double Double Double

-- |  Spheric3D theta phi, where phi = 
-- polar angle measured from a fixed zenith direction
data Spheric3D = Spheric3D Angle Angle
```

Asetamme kuun säteeksi $r$ = 1737.1 km. Yksinkertaisimman muunnoksen kolmiulotteisesta koordinaatistosta kaksiulotteiseen koordinaatistoon saamme pudottamalla $x$-koordinaatin pois.

```haskell
r = 1737.1 

dropX (Point3D x y z) = Point y z

perspective = dropX

cartesian (Spheric3D lambda delta) = Point3D x y z
  where
    x = r * cos1 theta * sin1 phi
    y = r * sin1 theta * sin1 phi
    z = r * cos1 phi
    theta = lambda
    phi = (DEG 90) `subAngles` delta
```

Saamme hahmotelman leveyspiireistä pallon etupuoliskolla algoritmilla

```haskell
latitudes = [PolyLine [(perspective . cartesian)
  (Spheric3D (DEG l) (DEG d))
  | l <- lambda]
    | d <- delta]
  where
    delta = [-90,-75..90]
    lambda = [-90,-70..90]
```

Etupuoliskon pituuspiirit eli meridiaanit saamme algoritmilla

```haskell
meridians = [PolyLine [(perspective . cartesian)
  (Spheric3D (DEG l) (DEG d))
  | d <- delta]
    | l <- lambda]
  where
    delta = [-90,-80..90]
    lambda = [-90,-75..90]
```

Olemme esittäneet leveys- ja pituuspiirien muodostaman kuvion kuvassa \ref{fig:spheric}.

\begin{figure}[H]
\begin{center}
\includegraphics{spheric.pdf}
\caption{Karttapallon puolisko, jossa kuvattuna leveyspiirit ja pituuspiirit eli meridiaanit 15 asteen välein.}
\label{fig:spheric}
\end{center}
\end{figure}

## Vinoprojektion perspektiivimatriisi

Niin sanotussa *vinoprojektiossa* kuvaamme kaksi akselia suoraan kulmaan toistensa kanssa ja kolmannen akselin tiettyyn kulmaan näiden välillä.

\begin{figure}[!htbp]
\begin{center}
\includegraphics{axes-xyz.pdf}
\caption{Vinoprojektioiden perspektiivimatriiseja.}
\label{fig:axes-xyz}
\end{center}
\end{figure}

Kuvassa \ref{fig:axes-xyz} esiintyvät vakiot $a$, $b$, $c$ ja $d$ olemme määritelleet seuraavasti kulman $\alpha$ avulla:
 
$$\begin{aligned}
 a &= \sfrac{1}{2} \cdot \cos \alpha \\
 b &= \sfrac{1}{2} \cdot \sin \alpha \\
 c &= -a \\
 d &= -b
\end{aligned}$$

Matriiseista järjestysluvultaan parittomat antavat kuvauskoordinaatistoksi vasenkätisen ja parilliset oikeakätisen koordinaatiston. Haskell-kielelle muunnettuna voimme esittää perspektiivimatriisit $M_{1..12}$ `case`-lauseen avulla.

```haskell
matrix1 m alpha = case m of
  1 -> [ [1,0,a], [0,1,b], z]
  2 -> [ [1,a,0], [0,b,1], z]
  3 -> [ [a,1,0], [b,0,1], z]
  4 -> [ [0,1,a], [1,0,b], z]
  5 -> [ [0,a,1], [1,b,0], z]
  6 -> [ [a,0,1], [b,1,0], z]
  7 -> [ [1,c,0], [0,d,1], z]
  8 -> [ [1,0,c], [0,1,d], z]
  9 -> [ [0,1,c], [1,0,d], z]
  10 -> [ [c,1,0], [d,0,1], z]
  11 -> [ [c,0,1], [d,1,0], z]
  12 -> [ [0,c,1], [1,d,0], z]
  where
    a = 0.5 * cos1 alpha
    b = 0.5 * sin1 alpha
    c = -a; d = -b
    z = [0,0,0]
```

Asetamme nyt muunnosmatriiseiksi matriisit $M_2$ ja $M_8$ (kuva \ref{fig:perspective-2}).

$$M_2 = \begin{pmatrix}
    1 & \sfrac{1}{2} \cdot \cos 30^{\circ} & 0 \\
    0 & \sfrac{1}{2} \cdot \sin 30^{\circ} & 1\\
    0 & 0 & 0 
\end{pmatrix}
\qquad{}
M_8 = \begin{pmatrix}
    1 & 0 & -\sfrac{1}{2} \cdot \cos 35^{\circ} \\
    0 & 1 & -\sfrac{1}{2} \cdot \sin 35^{\circ} \\
    0 & 0 & 0 
\end{pmatrix}$$

\begin{figure}[H]
\begin{center}
\includegraphics{perspective-2.pdf}
\qquad{}
\includegraphics{perspective-5.pdf}
\caption{Karttapallon puoliskot muunnosmatriiseja $M_2$ ja $M_8$ käyttäen.}
\label{fig:perspective-2}
\end{center}
\end{figure}

```haskell
matrixTimes3 a b = 
  [sum [x * y | (x,y) <- zip a1 b] | a1 <- a]

matr1 pv pAlpha (Point3D x1 y1 z1) = Point x y
  where
    [x,y,z] = matrixTimes3 (matrix1 pv alpha) [x1,y1,z1]
    alpha = DEG pAlpha

r = 1737.1 

dropX (Point3D x y z) = Point y z

perspective = matr1 pv pAlpha
  where
    pv = 8
    pAlpha = 35
```

## Mare Serenitatis

Haluamme seuraavaksi kuvata Hiljaisuuden meren karttapallolle. Kraatterin läpimitta on $d$ = 707 km, ja säde näin ollen $r = d/2$ = 353.5 km. Hiljaisuuden meren keskipisteen koordinaatit ovat ($28.0^{\circ}$ N, $17.5^{\circ}$ E).

Kuun säteen ollessa $r$ = 1737.1 km, saamme kuvan \ref{fig:mare-serenitatis-1} merkinnöillä keskuskulmaksi 
$$\theta = 2 \pi \cdot \frac{353.5} {2 \pi \cdot 1737.1} = 0.2035 \textrm{ rad} = 11.65^{\circ}$$

\begin{figure}[H]
\begin{center}
\includegraphics{mare-serenitatis-1.pdf}
\caption{Kraatterin säteen $r$ = 353.5 km muodostama keskuskulma.}
\label{fig:mare-serenitatis-1}
\end{center}
\end{figure}

Aiemmin esitellyn perusteella osaamme jo sijoittaa pohjoisnavalle ympyrän, jonka säde on annettu. Käytämme tällöin Hiljaisuuden merelle laskemaamme keskuskulmaa ylimääräisenä leveyspiirinä, jonka piirrämme täytettynä monikulmiona (kuva \ref{fig:mare-serenitatis-2}).

\begin{figure}[htbp]
\begin{center}
\includegraphics{mare-serenitatis-2.pdf}
\caption{Hiljaisuuden meri pohjoisnavalle sijoitettuna.}
\label{fig:mare-serenitatis-2}
\end{center}
\end{figure}

```haskell
serenitatis = [ Filled $ Polygon [
  (perspective . cartesian) (Spheric3D (DEG l) t)
  | l <- lambda]]
  where
    t = RAD (halfpi - 0.2035)
    lambda = [-180,-160..160]
```

## Kiertomatriisit kolmessa ulottuvuudessa

Kiertomatriisit kolmessa ulottuvuudessa kulman $\theta$ verran akselien $x$, $y$ ja $z$ suhteen ovat (<https://en.wikipedia.org/wiki/Rotation_matrix>)

$$R_x = \begin{pmatrix}
  1 & 0 & 0 \\
  0 & \cos \theta & -\sin \theta \\
  0 & \sin \theta & \cos \theta 
\end{pmatrix}
\qquad{}
R_y = \begin{pmatrix}
  \cos \theta & 0 & \sin \theta \\
  0 & 1 & 0 \\
  -\sin \theta & 0& \cos \theta 
\end{pmatrix}$$
$$R_z = \begin{pmatrix}
  \cos \theta & -\sin \theta & 0 \\
  \sin \theta & \cos \theta & 0 \\ 
  0 & 0 & 1 \\
\end{pmatrix}$$

Haskell-kielelle muunnettuna nämä ovat

```haskell
rotationX t = [
  [1,      0,       0],
  [0, cos1 t, -sin1 t],
  [0, sin1 t,  cos1 t]
  ] 

rotationY t = [
  [cos1 t,  0, sin1 t],
  [0,       1,      0],
  [-sin1 t, 0, cos1 t]
  ] 

rotationZ t = [
  [cos1 t, -sin1 t, 0],
  [sin1 t,  cos1 t, 0],
  [     0,       0, 1]
  ]
```

Latitudin $\delta$ komplementtikulma $\phi = 90^{\circ} - \delta$ määrittää kierron $y$-akselin suhteen ja longitudi $\lambda$ kierron $z$-akselin suhteen.

```haskell
rotYZ delta lambda (Point3D x1 y1 z1) = Point3D x y z
  where
    [x,y,z] = foldr matrixTimes3 [x1,y1,z1] rts
    rts = [rotationZ lambda,rotationY phi]
    phi = DEG 90 `subAngles` delta
```

Valitsemme perspektiivimatriisin $M_{10}$.

```haskell
perspective = matr1 pv pAlpha
  where
    pv = 10
    pAlpha = 35
```

Yleisessä muodossaan määrittelemme kuun meren piirtoalgoritmin funktiossa `mare`, joka saa parametrinaan `d` meren halkaisijan ja parametrinaan `pos` maantieteellisen pohjois-itä-koordinaatin tyyppiä `GeographicNE`.

```haskell
data GeographicNE = GeographicNE Angle Angle

mare d pos = [ Filled $ Polygon [
  (perspective .  rotYZ delta lambda . cartesian) 
    (Spheric3D (DEG l) phi)
  | l <- lambdaRim]]
  where
    GeographicNE delta lambda = pos
    phi = DEG 90 `subAngles` (RAD theta)
    theta = (d/2) / r
    lambdaRim = [-180,-160..160]
```

Hiljaisuuden meri saa nyt muodon

```haskell
serenitatis = mare d pos
  where
    d = 707 
    pos = GeographicNE (DEG 28) (DEG 17.5)
```

Piirrämme kuvaan \ref{fig:serenitatis-3} myös Myrskyjen valtameren, 
jonka halkaisija on $d$ = 2568 km, 
ja jonka keskipiste sijaitsee pisteessä ($18.4^{\circ}$ N, $57.4^{\circ}$ W).

\begin{figure}[htbp]
\begin{center}
\includegraphics{serenitatis-3.pdf}
\caption{Hiljaisuuden meri ja Myrskyjen valtameri.}
\label{fig:serenitatis-3}
\end{center}
\end{figure}

```haskell
procellarum = mare d pos
  where
    d = 2568 
    pos = GeographicNE (DEG 18.4) (DEG (-57.4))
```

Merkitsemme myös koordinaatiston nollapisteen funktiolla `proto0`.

```haskell
proto0 = mare 160 (GeographicNE (DEG 0) (DEG 0))
```

## Monikulmion paloittelu

Olemme koordinaattimuunnoksissa huomioineet ainoastaan täytetyn monikulmion reunapisteet, joten esimerkiksi Myrskyjen valtameren keskiosat piirtyivät väärin kuvassa \ref{fig:serenitatis-3}.

Parempaan tulokseen päädymme paloittelemalla monikulmiot asteverkon mukaisesti. Käytämme aluksi tasavälistä lieriöprojektiota (*equirectangular projection*), jossa pituus- ja leveysasteet kuvautuvat sellaisenaan koordinaattipisteiksi.

```haskell
equirectangular (Spheric3D lambda delta) = Point l d
  where
    DEG l = degrees lambda
    DEG d = degrees delta
```

Mittakaavakertoimena on seuraavassa $\dfrac{2 \pi \cdot r}{360}$, missä $r$ = 1737.1 km on kuun säde.

```haskell
mare d pos = [Polygon [ pt0 `addCoords`
  pointFromPolar (DEG l) r2 | l <- lambdaRim]]
  where
    r2 = (d/2) / (twopi * r / 360)
    pt0 = equirectangular (Spheric3D lambda delta)
    GeographicNE delta lambda = pos
    lambdaRim = [-180,-140..140]
```

Muunnamme polaarikoordinaatit pisteeksi funktiolla `pointFromPolar`.

```haskell
pointFromPolar t s = Point x y
  where
    x = s * cos1 t
    y = s * sin1 t
```

Olemme kuvassa \ref{fig:fecunditatis-1} esittäneet suorakulmaisessa koordinaatistossa Hedelmällisyyden meren, jonka halkaisija on $d$ = 909 km, ja jonka keskipiste sijaitsee pisteessä ($7.8^{\circ}$ S, $51.3^{\circ}$ E).

\begin{figure}[htbp]
\begin{center}
\includegraphics{fecunditatis-1.pdf}
\caption{Hedelmällisyyden meri suorakulmaisessa koordinaatistossa.}
\label{fig:fecunditatis-1}
\end{center}
\end{figure}

```haskell
fecunditatis = mare d pos
  where
    d = 909 
    pos = GeographicNE (DEG (-7.8)) (DEG 51.3)
```

## Pisteet monikulmion sisä- ja ulkopuolella

Käytämme monikulmion paloitteluun *Sutherland-Hodgmanin algoritmia*
(<https://en.wikipedia.org/wiki/Sutherland-Hodgman_algorithm>). Paloittelussa muokkaamme monikulmion kärkipistejoukkoa vertaamalla sitä leikkaavan monikulmion sivuihin sivu kerrallaan. Leikkauksen lähtöjoukkona toimii aina edellisessä vaiheessa saatu kärkipistejoukko. Kukin sivu leikkaa osan kärkipisteistä pois sekä muodostaa uusia kärkipisteitä paloiteltavan ja leikkaavan monikulmion sivujen leikkauspisteisiin.  Paloittelualgoritmia varten tarvitsemme tiedon siitä, kummalla puolella annettua sivua tietty kärkipiste sijaitsee.

Saamme selville kummalla puolella sivua piste sijaitsee muodostamalla kolmion, jonka kärkipisteet ovat sivun alkupiste, sivun loppupiste ja vertailtava piste. Kun piste sijaitsee sivun oikealla puolella, muodostuneen kolmion kiertosuunta on myötäpäivään, jolloin sen ala determinanttisäännön mukaan on negatiivinen. Kun piste sijaitsee sivun vasemmalla puolella, kiertosuunta on vastapäivään ja muodostuneen kolmion ala positiivinen.

```haskell
data InOut = In | Out
  deriving Show

sign x = if x < 0 then (-1) else 1
around xs = zip xs ((tail . cycle) xs)

inOut1 p1 p2 pg = [
  (inOut . sign . area . Polygon) [p1,p2,p3] | p3 <- pts]
  where
    Polygon pts  = pg
    inOut   1  = In
    inOut (-1) = Out

inOut2 ct pg = [ inOut1 p1 p2 pg 
   | (p1,p2) <- zip ct (tail ct)]

insideOutside pg = 
  inOut2 (head gridGreatCircles) pg 

gridGreatCircles = concat [[[
  xpt l1 d1, xpt l2 d1, xpt l2 d2, xpt l1 d2]
  | (d1,d2) <- zip vb3 (tail vb3)]
    | (l1,l2) <- zip vb2 (tail vb2)]
  where
    xpt l d = equirectangular (Spheric3D (DEG l) (DEG d))
    vb3 = visible3 delta
    vb2 = visible2 lambda
    delta = [-90,-75..90]
    lambda = [-90,-75..90]

visible2 = filter (\l -> l > 29 && l < 76)
visible3 = filter (\d -> d > -31 && d < 16)
```

Determinanttisääntöä käytämme, kun määrittelemme funktion `area` monikulmiolle `Polygon`. Determinantin saamme matematiikasta tutulla kaavalla
$$\det \begin{pmatrix}
  a & b \\
  c & d
\end{pmatrix} = a \cdot d - b \cdot c$$

```haskell
-- | http://mathworld.wolfram.com/PolygonArea.html
-- Polygon area: vertices counterclockwise
area (Polygon pts) = half (sum [det [[x1,y1],[x2,y2]] 
  | (Point x1 y1,Point x2 y2) 
    <- zip pts (tail pts ++ take 1 pts)])
 
half = (0.5 *)
det [[a,b],[c,d]] = a * d - b * c
```

Aloitamme kuvion paloittelun neliöstä alueen vasemmassa alanurkassa (kuva \ref{fig:fecunditatis-2}).

\begin{figure}[htbp]
\begin{center}
\includegraphics{fecunditatis-2.pdf}
\caption{Ensimmäinen tarkasteltava ruutu.}
\label{fig:fecunditatis-2}
\end{center}
\end{figure}

Ensimmäinen rajaava suora on neliön alareuna $s_1 s_2$. Alareuna säilyttää kaikki monikulmion pisteet.

```haskell
> insideOutside !! 0
[In,In,In,In,In,In,In,In,In]
```

Toinen rajaava suora on neliön oikea reuna $s_2 s_3$. Nyt rajaavan suoran vasemmalle puolelle eli alueen sisäpuolelle jäävät monikulmion pisteet $(p_1, p_2, p_9)$. Alueen ulkopuolelle jäävät monikulmion pisteet $(p_{3..8})$.

```haskell
> insideOutside !! 1
[In,In,Out,Out,Out,Out,Out,Out,In]
```

Paloiteltavan monikulmion sivut jakautuvat neljään ryhmään:

- `(In,In)`: sivu alkaa sisäpuolelta ja päättyy sisäpuolelle.
- `(In,Out)`: sivu alkaa sisäpuolelta ja päättyy ulkopuolelle.
- `(Out,Out)`: sivu alkaa ulkopuolelta ja päättyy ulkopuolelle.
- `(Out,In)`: sivu alkaa ulkopuolelta ja päättyy sisäpuolelle.

Monikulmion kaikki sivut suhteessa rajaavan neliön oikeaan reunaan $s_2 s_3$ muodostavat listan

```haskell
> around (insideOutside !! 1)
[ (In,In),(In,Out),(Out,Out),(Out,Out),(Out,Out),
  (Out,Out),(Out,Out),(Out,In),(In,In) ]
```

Sutherland-Hodgmanin algoritmin mukaiset toimenpiteet sivutyypeille ovat 

- `(In,In)`: säilytämme kärkipisteet.
- `(In,Out)`: säilytämme lähtöpisteen ja siirrämme loppupisteen.
- `(Out,Out)`: poistamme kärkipisteet.
- `(Out,In)`: siirrämme alkupisteen ja säilytämme loppupisteen.

Kuvan \ref{fig:fecunditatis-4} merkinnöillä suoran $s_2 s_3$ suhteen leikattu toinen monikulmio koostuu kärkipisteistä $(p_1, p_2, i_1, i_2, p_9)$.

\begin{figure}[htbp]
\begin{center}
\includegraphics{fecunditatis-4.pdf}
\caption{Toinen leikkaus antaa monikulmion kärkipisteet $(p_1, p_2, i_1, i_2, p_9)$.}
\label{fig:fecunditatis-4}
\end{center}
\end{figure}

```haskell
fecunditatis3 = Polygon fc3

fc3 = concat [new io1 io2 p1 p2 
  | ((io1,io2),(p1,p2)) <- zip io ia]
  where
    (s1,s2) = zip g1 (tail g1) !! 1
    g1 = gridGreatCircles !! 0
    io = around (insideOutside !! 1)
    ia = around pg
    Polygon pg = fecunditatis
    new In  In  p1 p2 = [p1]
    new In  Out p1 p2 = [p1,
      fromJust (intersection s1 s2 p1 p2)]
    new Out Out p1 p2 = []
    new Out In  p1 p2 = [
      fromJust (intersection s1 s2 p1 p2)]
```

Kolmas leikkaus tapahtuu suoran $s_3 s_4$ suhteen edellä saadulle kärkipistejoukolle $(i_1, p_2, p_3, p_4, i_2)$. Leikattu monikulmio koostuu kärkipisteistä $(i_3, p_2, i_1, i_4)$ (kuva \ref{fig:fecunditatis-5}).

\begin{figure}[htbp]
\begin{center}
\includegraphics{fecunditatis-5.pdf}
\caption{Kolmas leikkaus antaa monikulmion kärkipisteet $(i_3, p_2, i_1, i_4)$.}
\label{fig:fecunditatis-5}
\end{center}
\end{figure}

Viimeinen leikkaus tapahtuu suoran $s_4 s_1$ suhteen. Leikkaus säilyttää kärkipistejoukon $(i_3, p_2, i_1, i_4)$ sellaisenaan.


