## Harjoitus 4

Tehtävänä on laatia kuvan \ref{h4-tehtava} mukainen kuvio mitoituksineen.

\begin{figure}[H]
\begin{center}
\includegraphics{boomerang-10.pdf}
\caption{Tehtävän 4 kuvio.}
\label{h4-tehtava}
\end{center}
\end{figure}

Johdamme aluksi algoritmin kulmien pyöristämiseen. 

Määrittelemme esimerkkinä viivajonot `polyline1` ja `polyline2` (kuva \ref{h4-two-polylines}), jotka ovat tyyppiä `PolyLine`.

```haskell
polyline1 = PolyLine [
  mkPoint (-80,-80), mkPoint (0,0), mkPoint(80,-40)]
polyline2 = PolyLine [
  mkPoint (-40,-80), mkPoint (40,0), mkPoint(120,-40)]
```

\begin{figure}[H]
\begin{center}
\includegraphics{boomerang-22.pdf}
\caption{Kaksi viivajonoa.}
\label{h4-two-polylines}
\end{center}
\end{figure}

Käytämme seuraavia apufunktioita viivajonon purkamiseksi viivoiksi, jotka ovat tyyppiä `Line`:

```haskell
explode (Polygon pts) = [ Line pa pb 
  | (pa,pb) <- zip pts (tail pts ++ cycle pts) ]
explode (PolyLine pts) = [ Line pa pb 
  | (pa,pb) <- zip pts (tail pts) ]
extractPoints (PolyLine pts) = [(x,y) | Point x y <- pts]
```

Pyöristyksen lopputuloksena saamme viiva-kaari-elementtejä. Määrittelemme niille tyypin `ArcLine`, joka koostuu kaarielementistä `ArcElem` ja viivaelementistä `LineElem`.

```haskell
data ArcLineElem = ArcElem Double Angle Angle
  | LineElem Point Point
  deriving Show

data Shape = 
  ...
  | ArcLine [ArcLineElem]
  ...
```

Muunnamme esimerkin viivajonot viiva-kaari-elementeiksi.

```haskell
toArcLine elems =  
  ArcLine [toArcLine1 e | e <- elems]

toArcLine1 (Line p1 p2) = LineElem p1 p2
toArcLine1 (Arc r p start end) = ArcElem r start end

arcline1 = toArcLine (explode polyline1)
arcline2 = toArcLine (explode polyline2)
```

Määrittelemämme elementit ovat nyt

```haskell
> polyline1
PolyLine [
  Point (-80.0) (-80.0),Point 0.0 0.0,Point 80.0 (-40.0)]
> polyline2
PolyLine [
  Point (-40.0) (-80.0),Point 40.0 0.0,Point 120 (-40)]
> arcline1
ArcLine [
  LineElem (Point (-80.0) (-80.0)) (Point 0.0 0.0),
  LineElem (Point 0.0 0.0) (Point 80.0 (-40.0))]
> arcline2
ArcLine [
  LineElem (Point (-40.0) (-80.0)) (Point 40.0 0.0),
  LineElem (Point 40.0 0.0) (Point 120.0 (-40.0))]
```

Kuvan \ref{h4-pyoristaminen} pisteet $p_1$ ja $p_2$ ovat viiva-kaari-elementin ensimmäisen viivan päätepisteet ja pisteet $p_3$ ja $p_4$ toisen viivan päätepisteet.

Pyöristysympyrän keskipiste sijaitsee pisteessä $p$, jonka saamme säteen $r$ mittaisten normaalien etäisyydellä kulkevien viivojen leikkauspisteenä. Pyöristyskaaren alkupiste (`start`) ja loppupiste (`end`) ovat edelleen säteen etäisyydellä normaalia vastakkaiseen suuntaan kuljettaessa pisteestä $p$. 

\begin{figure}[H]
\begin{center}
\includegraphics{h4-pyoristaminen.pdf}
\caption{Kulman pyöristäminen.}
\label{h4-pyoristaminen}
\end{center}
\end{figure}

```haskell
data NormalLeftRight = NormalToLeft | NormalToRight 

normalVector dir r p1 p2 = 
  vectorFromAngle r (a1 dir)
  where
    a1 NormalToLeft  = t1 + halfpi
    a1 NormalToRight = t1 - halfpi
    v1 = mkVector p1 p2
    t1 = angleBt axisX v1

roundingElems r turn p1 p2 p3 p4 = 
  [ LineElem p1 pStart,
    ArcElem r (RAD start) (RAD end), 
    LineElem pEnd p4 ]
  where
    pEnd = toPoint p oppositeNormal2
    pStart = toPoint p oppositeNormal1
    p = fromJust pJust
    pJust = intersection p5 p6 p7 p8
    start = angleBt axisX oppositeNormal1 
    end = start + turn 
    oppositeNormal2 = opposite normal2
    oppositeNormal1 = opposite normal1
    p8 = toPoint p7 v2
    v2 = mkVector p3 p4
    p7 = toPoint p3 normal2
    normal2 = normalVector dir r p3 p4
    p6 = toPoint p5 v1
    v1 = mkVector p1 p2
    p5 = toPoint p1 normal1
    normal1 = normalVector dir r p1 p2
    dir
      | turn < 0  = NormalToRight
      | otherwise = NormalToLeft

roundedLL r (LineElem p1 p2) (LineElem p3 p4) = newElems
  where
    newElems = roundingElems r turn p1 p2 p3 p4 
    turn = angleBt (mkVector p1 p2) (mkVector p3 p4) 
```

Kahden viivan leikkauspisteen saamme seuraavalla algoritmilla (<https://en.wikipedia.org/wiki/Line-line_intersection>):

```haskell
intersection (Point x1 y1) (Point x2 y2) 
  (Point x3 y3) (Point x4 y4) 
  | parallel = Nothing
  | otherwise = Just (Point x y)
  where   
    x  = nx / d
    y  = ny / d
    nx = (x1*y2-y1*x2) * (x3-x4) - (x1-x2) * (x3*y4-y3*x4)
    ny = (x1*y2-y1*x2) * (y3-y4) - (y1-y2) * (x3*y4-y3*x4)
    d  = (x1-x2) * (y3-y4) - (y1-y2) * (x3-x4)
    parallel = (d == 0)
```

Pyöristämme viiva-kaari-elementin `arcline2` 20-säteisen pyöristysympyrän avulla (kuva \ref{h4-elementit}).

```haskell
rnd1 = rounded 20 arcline2

> rnd1
ArcLine [
  LineElem (Point (-40.0) (-80.0)) (Point 40.0 0.0),
  LineElem (Point 0.0 (-40.0)) (Point 37.4517 (-2.5483)),
  ArcElem 5.0 (RAD 2.35619449019) (RAD 1.107148717794),
  LineElem (Point 43.2233 (-1.6117)) (Point 80 (-20)),
  LineElem (Point 40.0 0.0) (Point 120.0 (-40.0))]
```

\begin{figure}[H]
\begin{center}
\includegraphics{boomerang-23.pdf}
\caption{Pyöristämätön ja pyöristetty kaari-viiva-elementti.}
\label{h4-elementit}
\end{center}
\end{figure}

Suoran ja ympyränkaaren välinen kulma pyöristyy vastaavalla tavalla (kuva \ref{h4-linearc-rounding}). Pyöristysympyrän keskipiste $p$ on nyt pisteet $p_3$ ja $p_4$ yhdistävän suoran ja alkuperäisen kaaren keskipisteestä $p_5$ piirretyn ympyrän leikkauspiste, kun jälkimmäisen ympyrän säde on kaaren säteen ja pyöristysympyrän säteen summa.

\begin{figure}[H]
\begin{center}
\includegraphics{h4-linearc-rounding.pdf}
\caption{Suoran ja ympyränkaaren välisen kulman pyöristäminen.}
\label{h4-linearc-rounding}
\end{center}
\end{figure}

Viiva-kaari-elementin pyöristyksen yhteydessä etsimme olemassaolevalle kaarelle uuden alkukulman. Se on vanhaa alkukulmaa lähinnä oleva täyden kierroksen kulma pisteiden $p$ ja $p_5$ väliseltä säteeltä, missä piste $p_5$ on alkuperäisen kaaren keskipiste ja piste $p$ pyöristysympyrän keskipiste.

```haskell
roundedLA r (LineElem p1 p2) (ArcElem r1 start1 end1) = 
  [ LineElem p1 pStart,
    ArcElem r (RAD startAngle) (RAD endAngle), 
    ArcElem r1 (RAD newStart) end1 ]
  where
    p4 = toPoint p3 v1
    v1 = mkVector p1 p2
    p3 = toPoint p1 normal1
    normal1 = normalVector dir r p1 p2
    dir
      | start1 < end1 = NormalToRight
      | otherwise = NormalToLeft
    p5 = elementCenter p2 (ArcElem r1 start1 end1)
    c2 = Circle (r1+r) p5  
    pts2 = circleLineIntersections c2 p3 p4
    p = head (sortByDistanceFrom (midway p3 p4) pts2)
    pToStart = mkVector p3 p1
    startAngle = angleBt axisX pToStart
    pStart = toPoint p pToStart
    d = angleBt pToStart (mkVector p p5)
    endAngle = startAngle + d
    pToEnd = vectorFromAngle r endAngle
    pEnd = toPoint p pToEnd
    newNormal = mkVector p5 p
    p8 = toPoint pEnd newNormal
    newStart = closestFullTurn newStart1 radStart1
    RAD radStart1 = start1
    newStart1 = angleBt axisX newNormal

closestFullTurn alpha beta = b1
  where 
    (a1,b1) = head s
    s = sort [(abs (alpha+a-beta),alpha+a) 
      | a <- fullturns]
    fullturns = [twopi * t | t <- [-3,-2,-1,0,1,2,3]]
```

Kuvion kaikkien kulmien pyöristämiseen käytämme elementtien puolittamista ja seuraavaa pyöristysrutiinien kokoelmaa erilaisille elementtien välisille kulmatyypeille:

```haskell
rounded r (Closed (ArcLine elems)) = ArcLine xs
  where
    xs = concat xss
    xss = [ roundedElem r e f | [e,f] <- chunksOf 2 
      (tail halves ++ [head halves]) ]
    halves = concat [halve elem | elem <- elems]

rounded r (ArcLine elems) = ArcLine xs
  where
    xs = [head halves] ++ concat xss ++ [last halves]
    xss = [ roundedElem r e f | [e,f] <- chunksOf 2 
      (tail halves) ] 
    halves = concat [halve elem | elem <- elems]

roundedElem r (LineElem p1 p2) (LineElem p3 p4) = 
  roundedLL r (LineElem  p1 p2) (LineElem p3 p4)

roundedElem r (LineElem p1 p2) (ArcElem r1 start1 end1) = 
  roundedLA r (LineElem p1 p2) (ArcElem r1 start1 end1) 

roundedElem r (ArcElem r1 start1 end1) (LineElem p1 p2) = 
  roundedAL r (LineElem p2 p1) (ArcElem r1 end1 start1)

roundedElem r e f = [e,f]
```

Suorien ja kaarien puolittamiseksi määrittelemme funktion `halve`.

```haskell
midway (Point x1 y1) (Point x2 y2) = 
  Point ((x1+x2)/2) ((y1+y2)/2)

halve (LineElem p1 p2) = 
  [LineElem p1 mid, LineElem mid p2]
  where
     mid = midway p1 p2

halve (ArcElem r start end) =
  [ArcElem r start mid, ArcElem r mid end]
  where 
    mid = RAD ((start1 + end1) / 2)
    RAD start1 = start
    RAD end1 = end
```

Suoran ja ympyrän leikkauspisteen laskemiseen käytämme seuraavaa algoritmia (<http://mathworld.wolfram.com/Circle-LineIntersection.html>):

```haskell
circleLineIntersections1 r (Point x1 y1) (Point x2 y2)
  | discr < 0  = []
  | discr == 0 = [Point x3 y3]
  | discr > 0  = [Point x3 y3, Point x4 y4]
  where
    sqr x = x * x
    dx = x2 - x1
    dy = y2 - y1
    dr = sqrt ((sqr dx) + (sqr dy))
    det = x1 * y2 - x2 * y1
    sign x 
      | x < 0  = (-1)
      | otherwise = 1
    discr = sqr r * sqr dr - sqr det
    x3 = (det * dy + sign dy * dx * sqrt discr) / (sqr dr)
    y3 = ((-det) * dx + abs dy * sqrt discr) / (sqr dr)
    x4 = (det * dy - sign dy * dx * sqrt discr) / (sqr dr)
    y4 = ((-det) * dx - abs dy * sqrt discr) / (sqr dr)
```

Viiva-kaari-elementti pyöristyy kuvan \ref{h4-viiva-kaari} osoittamalla tavalla.

\begin{figure}[H]
\begin{center}
\includegraphics{boomerang-24.pdf}
\caption{Alkuperäinen viiva-kaari-elementti ja sen pyöristetty muunnelma.}
\label{h4-viiva-kaari}
\end{center}
\end{figure}

Kaari-viiva-elementin pyöristämiseen käytämme käänteistä viiva-kaari-elementin pyöristysalgoritmia.

```haskell
roundedElem r (ArcElem r1 start1 end1) (LineElem p1 p2) = 
  roundedAL r (LineElem p2 p1) (ArcElem r1 end1 start1)

roundedAL r (LineElem p1 p2) (ArcElem r1 start1 end1) =
  [ ArcElem r2 end2 (RAD newStart),
    ArcElem newr (RAD endAngle) (RAD startAngle),
    LineElem pStart newp1 ]
  where
    [ LineElem newp1 pStart,
      ArcElem newr (RAD startAngle) (RAD endAngle), 
      ArcElem r2 (RAD newStart) end2 ] = rLA
    rLA = roundedLA r (LineElem p1 p2) 
      (ArcElem r1 start1 end1) 
```

Etsimme tehtävän kuviosta suorien ja ympyröiden leikkauspisteet ja järjestämme ne niiden etäisyyden mukaan lähtöpisteestä.

```haskell
sortByDistanceFrom p pts =
  [pts | (pts,ds) <- ordered]
  where
    ordered = sortBy (compare `on` snd) ptsDs
    ptsDs = zip pts ds
    ds = [dist pt p | pt <- pts]
```

Kuviosta löydämme odotetut kahdeksan leikkauspistettä (kuva \ref{h4-viiva-kaari-2}).

\begin{figure}[H]
\begin{center}
\includegraphics{boomerang-04.pdf}
\caption{Ympyröiden ja suorien leikkauspisteet.}
\label{h4-viiva-kaari-2}
\end{center}
\end{figure}

Seuraavaksi pilkomme viivan ja ympyrän niiden leikkauspisteistä ja nimeämme syntyneet osat (kuva \ref{h4-circle-line-explode}).

Nimetty kuvio (`Named`) voi olla mikä tahansa kuvio olemassaolevista kuvioiden (`Shape`) tietotyypeistä. Kuvion nimi on merkkijono `String`.

```
data Shape = 
  ...
  | Named String Shape
  ...
```

\begin{figure}[H]
\begin{center}
\includegraphics{boomerang-06.pdf}
\caption{Suoran ja ympyrän pilkkominen.}
\label{h4-circle-line-explode}
\end{center}
\end{figure}

Pilkomme tehtävän kuvion leikkauspisteistä ympyränkaariksi ja suoriksi kuvan \ref{h4-rect-circles-explode} mukaisesti. Siirrämme kuviot kuviotyypin mukaisesti omille tasoilleen.

Käyttämämme algoritmi on monivaiheinen.

```
pts1 = rectCircleIntersections rect1 circles1
exps1 = rectCircleExplosions rect1 circles1

rectCircleIntersections rect circles = 
  [Named (show n) (Node pt) | (n,pt) <- zip [1..] cpts]
  where
    cpts = concat pts
    pts = filter (not . null) ircs
    ircs = [circleLineIntersections circle p1 p2 
      | (Line p1 p2,circle) <- plc]
    plc = [(pl,c) | pl <- explode rect, c <- circles]

rectCircleExplosions rect circles = 
  [Named name o | (name,o) <- flatExALs]
  where
    flatExALs = concat (concat exBoth)
    exBoth = [onePlc o | o <- plc]
    plc = [(([name],line),c) 
      | (name,line) <- named2, c <- circles]
    named2 = zip ['a'..'z'] (explode rect)

onePlc ((name,Line p1 p2),circle) = 
  [oneArcs] ++ [oneLines]
  where
    oneArcs  = namedList 
      (length oneLines + 1) name 
      (explosionArcs circle p1 p2)
    oneLines = namedList 
      1 name (explosionLines circle p1 p2)

namedList start name lst = 
  [("$" ++ name ++ "_{" ++ show n ++ "}$",x) 
    | (n,x) <- zip [start..] lst]

explosionArcs circle p1 p2 = 
  cuts ircs
  where
    Circle r p = circle
    ircs = orderedCircleLineIntersections circle p1 p2 
    cuts [p3,p4] = mkArcs r p p3 p4
    cuts _ = []

mkArcs r p p1 p2 = 
  [Arc r p (RAD alpha1) (RAD c) 
    | c <- closests alpha1 (fullturns alpha2)]
  where
    v1 = mkVector p p1
    v2 = mkVector p p2
    delta = alpha2 - alpha1
    RAD alpha1 = angleBt axisX v1
    RAD alpha2 = angleBt axisX v2
    fullturns alpha = [alpha - twopi * t 
      | t <- [-3,-2,-1,0,1,2,3]]

closests beta alphas = 
  [b | (a,b) <- [s!!0,s!!1]]
  where 
    s = sort [(a,b) 
      | (a,b) <- zip [abs (a - beta) 
        | a <- alphas] alphas]

explosionLines circle p1 p2 = 
  cuts ircs
  where
    ircs = orderedCircleLineIntersections circle p1 p2 
    cuts [p3,p4] = [Line p1 p3] ++ [Line p3 p4] ++ 
      [Line p4 p2] 
    cuts _ = []

divideByShape y xs = filter (\x -> typeID x == y) xs
typeID (Named n (Arc r p a1 a2)) = 2
typeID (Named n (Line p1 p2)) = 1
```

\begin{figure}[H]
\begin{center}
\includegraphics{boomerang-07.pdf}
\caption{Suorakulmio ja ympyrät pilkottuina nimetyiksi elementeiksi.}
\label{h4-rect-circles-explode}
\end{center}
\end{figure}

Etsimämme kuvio syntyy seuraamalla polkua 

```
a1-a4-a3-b1-b4-b3-c1-c4-c3-d1-d4-d3
```

Määrittelemällä funktion `baseName` keräämme kuvion nimestä aakkosnumeeriset merkit (jättäen muotoilumerkit nimestä pois). Keräämme aakkosnumeerisen nimen ja kuviotiedon sisältävät parit kuvaukseksi `nameMap1`.

```haskell
baseName fullname = filter isAlphaNum fullname

nameMap1 xs = Map.fromList lst
  where
    lst = [(baseName n,shape) | Named n shape <- xs]
```

Kuvaus `nameMap1` kuvioista `exps1` on nyt muodoltaan seuraavan kaltainen:

```haskell
> nameMap1 exps1
fromList [
  ("a1",Line (Point (-52.5) (-53.5)) 
             (Point (-52.5) (-23.7))),
  ("a2",Line (Point (-52.5) (-23.7)) 
             (Point (-52.5) 23.7)),
  ("a3",Line (Point (-52.5) 23.7) 
             (Point (-52.5) 53.5)),
  ("a4",Arc 25.0 (Point (-60.5) 0.0) 
            (RAD (-1.245)) (RAD 1.245)),
  ("a5",Arc 25.0 (Point (-60.5) 0.0) 
            (RAD (-1.245)) (RAD (-5.038))),
  ("b1",Line (Point (-52.5) 53.5) 
             (Point (-30.0167) 53.5)),
  ...
```

Poimimme kuvauksesta valitsemaamme polkuun kuuluvat elementit ja muunnamme ne yhtenäiseksi viiva-kaari-jonoksi (kuva \ref{h4-selected-path}).

```haskell
arcline4 = arcLineFromEdgeList exps1 
  "a1-a4-a3-b1-b4-b3-c1-c4-c3-d1-d4-d3"

arcLineFromEdgeList exps1 edges = 
  toArcLine [shapes Map.! p | p <- path]
  where
    shapes = nameMap1 exps1
    path = splitOn "-" edges

toArcLine elems =  
  ArcLine [toArcLine1 e | e <- elems]

toArcLine1 (Line p1 p2) = LineElement [p1,p2]
toArcLine1 (Arc r p start end) = ArcElement r start end
```

\begin{figure}[H]
\begin{center}
\includegraphics{boomerang-08.pdf}
\caption{Kuvio viivojen ja kaarien muodostamana jonona.}
\label{h4-selected-path}
\end{center}
\end{figure}

Pyöristyksen lopputulos on kuvan \ref{h4-rounded} mukainen kuvio.

```haskell
roundedFinal1 = rounded 5 (Closed arcline4)
```

\begin{figure}[H]
\begin{center}
\includegraphics{boomerang-09.pdf}
\caption{Kuvion reunaviiva pyöristettynä.}
\label{h4-rounded}
\end{center}
\end{figure}

Annamme kuviolle seuraavan mitoituksen:

```haskell
annotations1 = [
  annotate "%.0f" (AnnotToUp 100 a),
  annotate "%.0f" (AnnotToUp 110 b),
  annotate "%.0f" (AnnotToRight 85 c),
  annotate "%.0f" (AnnotToRight 95 d),
  annotRadiusE "R%.1f" 35 (DEG 35) circle1,
  annotRadius "R%.0f" (DEG 240) circle2,
  annotRadius "R%.0f" (DEG 145) circle4 ]
  where
    a = [(-52.5,85), (52.5,85)]
    b = [(-60.5,85), (60.5,85)]
    c = [(72,-53.5), (72,53.5)]
    d = [(72,-71.5), (72,71.5)]
```

Kun lisäämme vielä koordinaattiristit ympyröiden keskipisteisiin oheisen listauksen mukaisesti, saamme kuvassa \ref{h4-annotated} esitetyn valmiin kuvion.

```
crosses1 = [(0,0),(-60.5,0),(60.5,0),(0,71.5),(0,-71.5)]
crosses = concat [
  [ Line (Point x (y-r)) (Point x (y+r)),
    Line (Point (x-r) y) (Point (x+r) y) ]
  |(x,y) <- crosses1]
  where
    r = 11

crossToCross i1 i2 = 
  shorten 13 13 
    (mkPoint (crosses1 !! i1)) 
    (mkPoint (crosses1 !! i2))

btCrosses1 = 
  [crossToCross a b | (a,b) <- [(0,1),(0,2),(0,3),(0,4)]]

shorten r1 r2 p1 p2 = Line p3 p4
  where
    p4 = toPoint p1 v3
    p3 = toPoint p1 v2
    v3 = vectorFromAngle (d-r2) alpha
    v2 = vectorFromAngle r1 alpha
    alpha = angleBt axisX v1
    v1 = mkVector p1 p2
    d = dist p1 p2
```

\begin{figure}[H]
\begin{center}
\includegraphics{boomerang-12.pdf}
\caption{Valmis kuvio.}
\label{h4-annotated}
\end{center}
\end{figure}


