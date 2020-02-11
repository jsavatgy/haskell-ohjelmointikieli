# Gtk-käyttöliittymä

Tässä luvussa tutustumme Gtk-käyttöliittymän kirjoittamiseen. Gtk (*the Gimp toolkit*) on suosittu käyttöliittymäkirjasto, joka aikoinaan syntyi Gimp-kuvankäsittelyohjelman komponenttikirjastona, ja joka nykyisin toimii useimpien Linux-koneiden työpöytäympäristön perusrakennusosana.

## Ohjelmaikkuna, jossa yksinkertainen painike

Ensimmäisessä esimerkkiohjelmassamme luomme ikkunan, joka sisältää yksinkertaisen painikkeen tekstillä "Click me!" (kuva \ref{button}).

\begin{figure}[ht]
\begin{center}
\includegraphics{click-me-bw.png}
\caption{Yksinkertainen painike.}
\label{button}
\end{center}
\end{figure}

```haskell
import Graphics.UI.Gtk
import Control.Monad.Trans (liftIO) 

main = do
  initGUI
  window <- windowNew
  button <- buttonNewWithLabel "Click me!"     
  containerAdd window button
  widgetShowAll window 
  button `on` buttonPressEvent $ tryEvent $ whenClicked
  onDestroy window mainQuit
  mainGUI

whenClicked = do
  liftIO $ putStrLn "Button was clicked."
```

Luomme ikkunan komennolla `windowNew` ja painikkeen komennolla `buttonNewWithLabel`. Liitämme painikkeen ikkunaan komennolla `containerAdd`. Esitämme ikkunan ja kaikki sen sisältämät alikomponentit komennolla `widgetShowAll`. Asetamme painikkeelle tapahtumankäsittelijän `whenClicked`. Ohjelma kutsuu tapahtumankäsittelijää, aina kun painike lähettää signaalin `buttonPressEvent` eli painiketta painettaessa. Ikkunan sulkeminen (`onDestroy`) päättää ohjelman suorituksen (`mainQuit`).

Tuomme kirjastosta `Control.Monad.Trans` funktion `liftIO`, jonka avulla voimme yhteensovittaa tapahtumankäsittelijän vaatiman tyypin `EventM` sekä syöte- ja tulostustyypin `IO`.

## Ohjelmaikkuna, jossa tekstinsyöttökenttä

Lisäsimme edellä ohjelmaikkunaan painikkeen komennolla `buttonNewWithLabel`. Myös muiden komponenttien lisäys tapahtuu samaa nimeämislogiikkaa noudattaen. Esimerkiksi yksinkertaisen tekstinsyöttökentän (`Entry`) lisäämme komennolla `entryNew` (kuva \ref{entry}).

\begin{figure}[ht]
\begin{center}
\includegraphics{sinus-lunicus-bw.png}
\caption{Tekstinsyöttökenttä.}
\label{entry}
\end{center}
\end{figure}

```haskell
import Graphics.UI.Gtk

main = do
  initGUI
  window <- windowNew
  entry <- entryNew     
  containerAdd window entry
  widgetShowAll window 
  onDestroy window mainQuit
  mainGUI
```

## Ohjelmaikkuna puunäkymällä

Puunäkymän lisääminen on hieman monimutkaisempi toimenpide, sillä puunäkymä tarvitsee näkymän (*view*) lisäksi myös tietomallin (*model*). Tietomallin pohjana voi olla yksinkertainen lista. Muodostamme listasta tietomallin komennolla `listStoreNew`.

Kun haluamme lisätä ikkunaan useampia komponentteja, on meidän päätettävä komponenttien asettelusta. Käytämme seuraavassa komponenttien asettelemiseksi allekkain pystysuuntaista laatikkoa `VBox` (*vertical box*). Asettelemme komponentit laatikkoon komennolla `boxPackStart` (kuva \ref{tree-view}).

\begin{figure}[ht]
\begin{center}
\includegraphics{tree-view-bw.png}
\caption{Tekstinsyöttökenttä ja puunäkymä.}
\label{tree-view}
\end{center}
\end{figure}

```haskell
import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as Model

main = do
  initGUI
  window <- windowNew
  vbox1 <- vBoxNew False 0
  entry <- entryNew     
  containerAdd window vbox1
  list <- listStoreNew [
    "Lacus Somniorum", "Lacus Doloris", "Lacus Timoris",
    "Palus Putredinis", "Lacus Autumni"]
  treeview <- Model.treeViewNewWithModel list
  Model.treeViewSetHeadersVisible treeview False
  col <- Model.treeViewColumnNew
  renderer <- Model.cellRendererTextNew
  Model.cellLayoutPackStart col renderer False
  Model.cellLayoutSetAttributes col renderer list
    (\text -> [Model.cellText := text])
  Model.treeViewAppendColumn treeview col
  tree <- Model.treeViewGetSelection treeview
  boxPackStart vbox1 entry PackRepel 0
  boxPackStart vbox1 treeview PackRepel 0
  widgetShowAll window 
  onDestroy window mainQuit
  mainGUI
```

## Tuloslistan suodattaminen säännöllisillä lausekkeilla

Kirjoitamme seuraavaksi ohjelman, joka suodattaa tuloslistan tekstikentässä antamamme säännöllisen lausekkeen avulla. 

Luemme ensin komennolla `readFile` tekstitiedoston, josta suodatamme pois tyhjät rivit.

Määrittelemme näppäimen vapautuksen (`keyReleaseEvent`) tapahtumankäsittelijäksi funktion `updateList1`, joka lukee tekstikentän sisällön, kun sitä on muutettu.

Säännöllisten lausekkeiden käsittelyyn tuomme kirjaston `Text.Regex.Posix`, joka tarjoaa funktion `(=~)`. Funktio `(=~)` on monimuotoinen ja vaatii siksi yleisessä muodossaan tyyppimäärittelyn. Kun valitsemme lausekkeen `a =~ b` tyypiksi `Bool`, palauttaa lauseke arvon `True`, mikäli säännöllinen lauseke `b` esiintyy merkkijonossa `a`. Mikäli valitsemme lausekkeen `a =~ b` tyypiksi `String`, lauseke palauttaa ensimmäisen osuman. Listauksessamme käytämme funktiota `(=~)` funktion `filter` ensimmäisenä argumenttina, joten kääntäjä päättelee tyypin olevan `Bool`.

```haskell
> import Text.Regex.Posix
> "abcd" =~ "a" :: Bool
True
> "abcd" =~ "a" :: String
"a"
```

Kuvassa \ref{list-store} olemme syöttäneet tekstikenttään säännöllisen lausekkeen `(.)\1`. Säännöllisissä lausekkeissa piste `.` vastaa mitä tahansa merkkiä. Sulkumerkit `()` muodostavat ensimmäisen alilausekkeen. Merkintä `\1` vastaa ensimmäistä alilauseketta. Kokonaisuudessaan säännöllinen lauseke `(.)\1` vastaa siten kahta peräkkäistä samaa merkkiä. Esimerkissämme tekstirivejä, joilla esiintyy kaksi peräkkäistä samaa merkkiä, löytyy yhteensä 10 kappaletta.

\begin{figure}[ht]
\begin{center}
\includegraphics{list-store-bw.png}
\caption{Tuloslista suodatettuna säännöllisellä lausekkeella \texttt{(.)\textbackslash{}1}.}
\label{list-store}
\end{center}
\end{figure}


## Poikkeuksien käsittely

Kun säännöllinen lauseke ei ole kelvollinen, nostaa järjestelmä poikkeuksen (*exception*). Esimerkiksi keskeneräiset lausekkeet kuten "`(.`" eivät ole kelvollisia. Poikkeuksien käsittelyyn tuomme kirjaston `Control.Exception`, joka tarjoaa muun muassa funktiot `try` ja `evaluate`. Näistä `try` on monimuotoinen funktio ja vaatii siksi yleisessä muodossaan tyyppimäärittelyn. Annamme tyyppimäärittelyn funktion `tryFilter` tyyppiallekirjoituksessa. 

```haskell
tryFilter :: String -> [String] 
          -> IO (Either SomeException [String])
tryFilter txt list = do 
  result <- try (evaluate (filter (=~ txt) list)) 
  return result
```

Tyypin `Either` mahdolliset arvot ovat `Left x` ja `Right y`. Funktio `try` palauttaa arvon `Left x` silloin, kun argumenttina antamamme funktion suoritus on keskeytynyt poikkeukseen `x` sekä arvon `Right y` silloin, kun antamamme funktio onnistuneesti palauttaa arvon `y`. Esimerkissämme samaistamme virheellisen säännöllisen lausekkeen lausekkeeseen, jonka tulosjoukko on tyhjä lista `[]`.

```haskell
filter1 txt list = do
  result <- tryFilter txt list
  return $ case result of
    Left ex -> []
    Right val -> val
```

Esitämme seuraavassa ohjelmakoodin kokonaisuudessaan.

\verbatiminput{code/list-store.hs}

## Ohjelma Png-kuvan näyttämiseen

Seuraavassa esimerkkiohjelmassa emme käytä moniakaan Gtk-kirjaston visuaalisia komponentteja vaan kirjoitamme lyhyen yleiskäyttöisen ohjelman Png-muotoisen kuvan esittämiseksi ruudulla.

\verbatiminput{code/png-view.hs}

## Piirtoalue `DrawingArea`

Luomme ikkunaan piirtoalueen komennolla `drawingAreaNew`. Annamme piirtoalueelle nimen `canvas`. Piirtoalueen kokopyynnön asetamme komennolla `widgetSetSizeRequest`.

Luemme piirtoalueelle kuvan määrittelemällä komennon `surfaceFromPNG`. Kuvan keskittämiseksi piirtoalueelle määrittelemme komennon `centerImg`.

Luomme tapahtumankäsittelijöiden avulla käyttäjälle mahdollisuuden siirtää kuvaa ja muuttaa kuvan kokoa. Kun kuvaa on siirretty tai sen kokoa muutettu, piirrämme kuvan uudestaan käyttäen määrittelemäämme komentoa `updateCanvas1`. Tämä komento kutsuu piirtotyön suorittavaa rutiinia `paintImage1`, jossa kutsumme piirtokirjasto Cairon tarjoamia piirtokomentoja. 

## Tapahtumankäsittelijät

Pääohjelmassa olemme määritelleet tapahtumankäsittelijän hiiren liikkeelle (`motionNotifyEvent`), näppäimen painallukselle (`keyPressEvent`), hiiren näppäimen painallukselle (`buttonPressEvent`), hiiren näppäimen vapauttamiselle (`buttonReleaseEvent`), hiiren rullan pyörittämiselle (`scrollEvent`), ikkunan sulkupainikkeen painamiselle (`onDestroy`) sekä piirtoalueen uudelleenpiirtämiselle (`onExpose`).

Osa tapahtumankäsittelijöistämme on hyvin yksinkertaisia, ja ne kirjoittavat ainoastaan viestin komentoikkunaan funktiokutsulla `logMsg 1` tai jättävät kirjoittamatta funktiokutsulla `logMsg 0`.

Olemme tuoneet kirjaston `Graphics.UI.Gtk.Gdk.EventM` nimettynä `(qualified ... as M)`, jolloin funktiokutsut saavat aina etuliitteen "`M.`".

Tavallisesti hiiren liikuttaminen piirtoalueella ei tuota tapahtumasignaalia, mutta voimme halutessamme tuottaa sellaisia esimerkiksi silloin, kun hiiren ykköspainike on painettuna. Teemme näin pääohjelmassa komennolla `widgetAddEvents`.

```haskell
widgetAddEvents canvas [Button1MotionMask]
```

Annamme ohjelmalle nimeksi png-view ja käynnistämme ohjelman komentotulkissa. Ohjelma saa argumenttinaan kuvatiedoston nimen.

```
$ runhaskell png-view foci-5.png
```

Avautuvassa ikkunassa näemme valitsemamme kuvan (kuva \ref{png-view-1}). Voimme suurentaa ja pienentää näkymää hiiren rullalla.

\begin{figure}[H]
\begin{center}
\includegraphics{png-view-foci-5-bw.png}
\caption{Ohjelmaikkuna.}
\label{png-view-1}
\end{center}
\end{figure}


## `MVar`-muuttujaviittaukset

Käytämme ohjelmassamme `MVar`-muuttujaviittauksia tapahtumankäsittelijöissä. Tämä on käytännöllinen tapa välittää tietoa graafisen käyttöliittymän sisällä. 

Luomme uuden muuttujan komennolla `newMVar`. Muuttujan sisältämän tiedon luemme komennolla `readMVar`. Olemassaolevaa muuttujaa muutamme komennolla `modifyMVar_`.

Käyttämämme muuttujaviittaukset ovat `var`, joka sisältää kuvan suurennoksen sekä vasemman ylänurkan $x$- ja $y$-koordinaatit, ja `vPos`, joka sisältää hiiritapahtuman syyn (`Press` (*painallus*), `Release` (*vapautus*), `Move` (*siirto*), `Scroll` (*rullaus*) tai `None` (*ei mikään*) sekä tapahtumahetken hiiren osoittimen $x$- ja $y$-koordinaatit.

Muuttujaviittausten sisältämän tiedon avulla ohjelma kykenee laskemaan esitettävälle kuvalle uudet koordinaatit ja suurennoksen aina tapahtumankäsittelijän sitä pyytäessä.

## Tekstinäkymä ja tekstipuskuri

Gtk-kirjaston tekstinäkymä (`textView`) ja tekstipuskuri (`textBuffer`) sisältävät hyvin monipuoliset välineet yksinkertaisen tekstimuokkaimen luomiseen.

Kirjoitamme lyhyen ohjelman tekstitiedoston muokkaamiseen (kuva \ref{text-view-1}).

\begin{figure}[htbp]
\begin{center}
\includegraphics{text-view-puulajit-bw.png}
\caption{Tekstitiedosto tekstinäkymäkomponentissa.}
\label{text-view-1}
\end{center}
\end{figure}

\verbatiminput{code/text-view.hs}

## Gtk-käyttöliittymäkirjastojen hierarkia

- `Graphics.UI.Gtk` 
  - `Abstract` 
    - `Bin`, `Box`, `ButtonBox`, `Container`, `IMContext`, `Misc`, `Object`, 
      `Paned`, `Range`, `Scale`, `Scrollbar`, `Separator`, `Widget`. 
  - `ActionMenuToolbar` 
    - `Action`, `ActionGroup`, `RadioAction`, `RecentAction`, `ToggleAction`, `UIManager`. 
  - `Builder` 
  - `Buttons` 
    - `Button`, `CheckButton`, `LinkButton`, `RadioButton`, `ScaleButton`, `ToggleButton`, `VolumeButton`. 
  - `Cairo` 
  - `Display` 
    - `AccelLabel`, `Image`, `InfoBar`, `Label`, `ProgressBar`, `Spinner`, `StatusIcon`, `Statusbar`. 
  - `Embedding` 
    - `Embedding`, `Plug`, `Socket`, `Types`. 
  - `Entry` 
    - `Editable`, `Entry`, `EntryBuffer`, `EntryCompletion`, `HScale`, `SpinButton`, `VScale`. 
  - `Gdk` 
    - `AppLaunchContext`, `Cursor`, `Display`, `DisplayManager`, 
      `DrawWindow`, `Drawable`, `EventM`, `Events`, `GC`, `Gdk`, `Keymap`, 
      `Keys`, `Pixbuf`, `PixbufAnimation`, `Pixmap`, `Region`, `Screen`. 
  - `General` 
    - `Clipboard`, `Drag`, `Enums`, `General`, `IconFactory`, `IconTheme`, 
      `RcStyle`, `Selection`, `Settings`, `StockItems`, `Style`. 
  - `Layout` 
    - `Alignment`, `AspectFrame`, `Expander`, `Fixed`, `HBox`, `HButtonBox`, 
      `HPaned`, `Layout`, `Notebook`, `Table`, `VBox`, `VButtonBox`, `VPaned`. 
  - `MenuComboToolbar` 
    - `CheckMenuItem`, `Combo`, `ComboBox`, `ComboBoxEntry`, `ImageMenuItem`, 
      `Menu`, `MenuBar`, `MenuItem`, `MenuShell`, `MenuToolButton`, `OptionMenu`, 
      `RadioMenuItem`, `RadioToolButton`, `SeparatorMenuItem`, `SeparatorToolItem`, 
      `TearoffMenuItem`, `ToggleToolButton`, `ToolButton`, `ToolItem`, 
      `ToolItemGroup`, `ToolPalette`, `Toolbar`. 
  - `Misc` 
    - `Accessible`, `Adjustment`, `Arrow`, `Calendar`, `DrawingArea`, `EventBox`, 
      `HandleBox`, `IMContextSimple`, `IMMulticontext`, `SizeGroup`, `Tooltip`, 
      `Tooltips`, `Viewport`. 
  - `ModelView` 
    - `CellEditable`, `CellLayout`, `CellRenderer`, `CellRendererAccel`, 
      `CellRendererCombo`, `CellRendererPixbuf`, `CellRendererProgress`, 
      `CellRendererSpin`, `CellRendererSpinner`, `CellRendererText`, 
      `CellRendererToggle`, `CellView`, `CustomStore`, `IconView`, 
      `ListStore`, `TreeDrag`, `TreeModel`, `TreeModelFilter`, 
      `TreeModelSort`, `TreeRowReference`, `TreeSelection`, `TreeSortable`, 
      `TreeStore`, `TreeView`, `TreeViewColumn`. 
  - `Multiline` 
    - `TextBuffer`, `TextIter`, `TextMark`, `TextTag`, `TextTagTable`, `TextView`. 
  - `Ornaments` 
    - `Frame`, `HSeparator`, `VSeparator`. 
  - `Printing` 
    - `PageSetup`, `PaperSize`, `PrintContext`, `PrintOperation`, `PrintSettings`. 
  - `Recent` 
    - `RecentChooser`, `RecentChooserMenu`, `RecentChooserWidget`, `RecentFilter`, 
      `RecentInfo`, `RecentManager`. 
  - `Scrolling` 
    - `HScrollbar`, `ScrolledWindow`, `VScrollbar`. 
  - `Selectors` 
    - `ColorButton`, `ColorSelection`, `ColorSelectionDialog`, `FileChooser`, 
      `FileChooserButton`, `FileChooserDialog`, `FileChooserWidget`, `FileFilter`, 
      `FileSelection`, `FontButton`, `FontSelection`, `FontSelectionDialog`, `HSV`.
  - `Special` 
    - `HRuler`, `Ruler`, `VRuler`. 
  - `Windows` 
    - `AboutDialog`, `Assistant`, `Dialog`, `Invisible`, `MessageDialog`, 
      `OffscreenWindow`, `Window`, `WindowGroup`. 


## Kirjasto `Graphics.UI.Gtk.Abstract.Widget`

Useimpien komponenttien (kuten `Button`) perustyyppi on `Widget`. Kun etsimme esimerkiksi painikkeelle tapahtumankäsittelijöitä, on meidän etsittävä niitä komponentin perustyypin dokumentaatiosta.

Luettelemme seuraavassa kirjaston `Graphics.UI.Gtk.Abstract.Widget` tarjoamat tyypit, metodit, attribuutit ja tapahtumat.

\begin{Verbatim}[baselinestretch=0.85,commandchars=\\\{\}]
\textrm{Tyypit: }
GType  KeyVal StockId   AccelFlags Requisition  WidgetHelpType
Color  Region EventMask Allocation ExtensionMode DirectionType
Widget Bitmap Rectangle  StateType TextDirection              

\textrm{Etuliite: }widget-
\textrm{Metodit: }
~Show     ~SizeAllocate ~SetDirection ~InputShapeCombineMask
~Hide     ~SetAccelPath ~GetDirection  ~TranslateCoordinates
~Path     ~SetSensitive ~CreateLayout  ~SetScrollAdjustments
~ShowNow  ~PushColormap ~GetClipboard   ~GetChildRequisition
~ShowAll   ~QueueResize ~SetNoShowAll   ~QueueResizeNoRedraw
~HideAll   ~GrabDefault ~GetNoShowAll   ~SetDefaultDirection
~Destroy   ~GetToplevel ~IsComposited   ~GetDefaultDirection
~SetName   ~GetAncestor ~KeynavFailed   ~SetRedrawOnAllocate
~GetName   ~GetColormap ~GetHasWindow   ~RemoveMnemonicLabel
~HasGrab   ~SetColormap ~SetHasWindow   ~TriggerTooltipQuery
~GetSize   ~SizeRequest ~GetSensitive    ~SetExtensionEvents
~Activate  ~PopColormap ~GetDrawWindow   ~GetExtensionEvents
~SetStyle  ~GetSnapshot ~QueueDrawArea   ~SetDefaultColormap
~GetStyle  ~ModifyStyle ~GetAccessible   ~GetDefaultColormap
~ModifyFg  ~RestoreText ~GetRootWindow   ~CreatePangoContext
~ModifyBg  ~RestoreBase ~GetHasTooltip   ~ListMnemonicLabels
~Reparent  ~ResetShapes ~SetHasTooltip   ~SetReceivesDefault
~GetState  ~GetSettings ~GetAllocation   ~GetReceivesDefault
~SetState  ~GetCanFocus ~GetCanDefault    ~RemoveAccelerator
~QueueDraw ~SetCanFocus ~SetCanDefault    ~SetDoubleBuffered
~Intersect ~IsSensitive ~GetHasDefault     ~CanActivateAccel
~GrabFocus ~GetHasFocus ~GetSavedState     ~ShapeCombineMask
~DelEvents  ~GetIsFocus ~AddAccelerator    ~GetCompositeName
~AddEvents  ~GetPointer ~SetSensitivity    ~GetModifierStyle
~GetEvents  ~IsAncestor ~GetSizeRequest    ~SetCompositeName
~SetEvents  ~ModifyText ~SetSizeRequest    ~MnemonicActivate
~ClassPath  ~ModifyBase ~GetTooltipText    ~AddMnemonicLabel
~RestoreFg  ~ModifyFont ~SetTooltipText    ~GetTooltipMarkup
~RestoreBg  ~RenderIcon ~HasIntersection   ~SetTooltipMarkup
~GetParent  ~ChildFocus ~GetParentWindow   ~GetTooltipWindow
~GetScreen  ~GetDisplay ~GetDefaultStyle   ~SetTooltipWindow
~HasScreen  ~GetVisible ~GetPangoContext    ~SetChildVisible
~GetAction  ~IsDrawable ~SetAppPaintable    ~GetAppPaintable
~ErrorBell  ~IsToplevel ~RegionIntersect                    
~GetWindow   ~SetWindow ~GetChildVisible                    

\textrm{Attribuutit: }
~Name   ~MarginLeft ~HasDefault ~ReceivesDefault
~Style  ~CanDefault ~HExpandSet ~ExtensionEvents
~State   ~MarginTop ~VExpandSet  ~CompositeChild
~Parent  ~Sensitive ~HasTooltip   ~HeightRequest
~Events  ~NoShowAll ~HasRcStyle   ~CompositeName
~Expand  ~Direction ~MarginRight  ~TooltipMarkup
~Visible ~GetMapped ~TooltipText   ~AppPaintable
~Opacity ~SetMapped ~GetRealized   ~ChildVisible
~IsFocus  ~CanFocus ~SetRealized                
~HExpand  ~HasFocus ~WidthRequest               
~VExpand  ~Colormap ~MarginBottom               

\textrm{Signaalit: }
focus    showSignal unmapSignal accelClosuresChanged
realize  hideSignal sizeRequest     hierarchyChanged
styleSet grabNotify sizeAllocate    directionChanged
showHelp  mapSignal stateChanged     popupMenuSignal
unrealize parentSet queryTooltip       screenChanged

\textrm{Tapahtumat: }
mapEvent  grabBrokenEvent keyReleaseEvent visibilityNotifyEvent
unmapEvent configureEvent buttonPressEvent   buttonReleaseEvent
deleteEvent focusOutEvent enterNotifyEvent    motionNotifyEvent
exposeEvent keyPressEvent leaveNotifyEvent    proximityOutEvent
scrollEvent noExposeEvent proximityInEvent                     
destroyEvent focusInEvent windowStateEvent                     
\end{Verbatim}

## Kirjasto `Graphics.UI.Gtk.Gdk.EventM`

Komponentin vastaanottamat tapahtumat on määritelty kirjastossa `Graphics.UI.Gtk.Abstract.Widget`. Tapahtumien sisältämä muu informaatio on löydettävissä kirjaston `Graphics.UI.Gtk.Gdk.EventM` tarjoamien funktioiden avulla. Luettelemme osan kirjaston tarjoamista funktioista seuraavassa.

\begin{Verbatim}[baselinestretch=0.85,commandchars=\\\{\}]
\textrm{Tyypit: }
EAny   EButton ECrossing EWindowState
EKey   EScroll EProperty EOwnerChange
EventM EMotion EConfigure EVisibility
EFocus EExpose EProximity EGrabBroken

\textrm{Etuliite: }event-
\textrm{Funktiot: }
~Sent ~NotifyType ~GrabWindow ~WindowStateChanged
~Time  ~Selection currentTime    ~RootCoordinates
~Area   ~Modifier ~Coordinates   ~HardwareKeycode
~Size   ~Position ~ModifierAll   ~ScrollDirection
~Window ~Implicit ~WindowState   ~VisibilityState
~KeyVal stopEvent ~CrossingMode    ~CrossingFocus
~Button  ~KeyName ~ChangeReason    ~SelectionTime
~IsHint  ~FocusIn ~KeyboardGrab                  
~Region  tryEvent ~KeyboardGroup                 
\end{Verbatim}


