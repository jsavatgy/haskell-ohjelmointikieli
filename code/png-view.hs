import Control.Concurrent.MVar 
import System.Environment (getArgs)
import Graphics.UI.Gtk 
import Graphics.Rendering.Cairo
import qualified Graphics.UI.Gtk.Gdk.EventM as M
import System.Glib.UTFString (glibToString)

firstArg args = 
  case args of
    [] -> error "must supply a file to open"
    [arg] -> arg
    _ -> error "too many arguments"

main = do
  args <- getArgs
  let arg1 = firstArg args
  initGUI
  var <- newMVar (1.0,0.0,0.0)
  vPos <- newMVar (None,0.0,0.0)
  window <- windowNew
  canvas <- drawingAreaNew
  surf <- surfaceFromPNG arg1
  widgetAddEvents canvas [Button1MotionMask] 
  widgetSetSizeRequest canvas 630 891
  centerImg var surf canvas 
  canvas `on` motionNotifyEvent $ do
    (mouseX,mouseY) <- eventCoordinates
    t <- M.eventTime
    liftIO $ 
      changePos vPos var surf canvas mouseX mouseY
    liftIO $ logMsg 0 ("Motion Time: " ++ s t)
    return False
  window `on` keyPressEvent $ tryEvent $ do
    key <- eventKeyName
    keyInput var surf canvas (glibToString key)
    liftIO $ updateCanvas1 var canvas surf
    return ()
  canvas `on` buttonPressEvent $ tryEvent $ do
    (mouseX,mouseY) <- printMouse
    liftIO $ printPointer canvas
    liftIO $ printMVar var mouseX mouseY
    liftIO $ modifyMVar_ vPos (\_ -> 
      return (Press,mouseX,mouseY)) 
  canvas `on` buttonReleaseEvent $ tryEvent $ do
    (mouseX,mouseY) <- M.eventCoordinates
    m <- M.eventModifier
    b <- M.eventButton
    (cause,vPosX,vPosY) <- liftIO $ readMVar vPos
    liftIO $ release cause b var vPosX vPosY
  canvas `on` scrollEvent $ tryEvent $ do
    (mouseX,mouseY) <- M.eventCoordinates
    m <- M.eventModifier
    d <- M.eventScrollDirection
    t <- M.eventTime
    liftIO $ changeRef var d mouseX mouseY
    liftIO $ updateCanvas1 var canvas surf
    liftIO $ logMsg 0 ("Scroll: " ++ s t ++ s mouseX ++
      s mouseY ++ s m ++ s d) 
  onDestroy window mainQuit
  onExpose canvas $ const (updateCanvas1 var canvas surf)
  set window [containerChild := canvas]
  widgetShowAll window
  mainGUI

data RGB = RGB Double Double Double
  deriving Show

setColor (RGB red grn blu) = do
  setSourceRGBA red grn blu 1.0

black      = RGB 0.00 0.00 0.00 
white      = RGB 1.00 1.00 1.00 
pink       = RGB 0.96 0.57 0.70
violet     = RGB 0.69 0.61 0.85
orange     = RGB 0.98 0.63 0.15
blue       = RGB 0.33 0.67 0.82
sand       = RGB 0.90 0.80 0.55
darkBrown  = RGB 0.67 0.45 0.27
gray n = RGB n n n

data EvtType = Press | Release | Move | Scroll | None

release Press button var mouseX mouseY = do
  (varS,varX,varY) <- readMVar var
  let
    x = (mouseX - varX) / varS
    y = (mouseY - varY) / varS
  liftIO $ logMsg 0 
    ("Add point: " ++ s x ++ s y ++ s button)
  liftIO $ logMsg 1 (s x ++ s y)

release _ button var x y = do
  liftIO $ logMsg 0 ("Release (other): " ++ s x ++ s y)

changePos vPos var surf canvas mouseX mouseY = do
  (cause,vPosX,vPosY) <- readMVar vPos
  (scaleOld,oldX,oldY) <- readMVar var
  let
    dx = vPosX - mouseX
    dy = vPosY - mouseY
  modifyMVar_ var (\_ -> 
    return (scaleOld,oldX - dx,oldY - dy))
  modifyMVar_ vPos (\_ -> 
    return (Move,mouseX,mouseY))
  updateCanvas1 var canvas surf

intToDouble :: Int -> Double
intToDouble i = fromRational (toRational i)

s x = show x ++ " "

printMouse = do
  (mouseX,mouseY) <- M.eventCoordinates
  liftIO $ logMsg 0 ("Mouse: " ++ s mouseX ++ s mouseY)
  return  (mouseX,mouseY)

printPointer canvas = do
  (widX,widY) <- widgetGetPointer canvas
  logMsg 0 ("Widget: " ++ s widX ++ s widY)

printMVar var mouseX mouseY = do
  (varS,varX,varY) <- readMVar var
  let
    x = (mouseX - varX) / varS
    y = (mouseY - varY) / varS
  logMsg 0 ("MVar: " ++ s varS ++ s varX ++ s varY)
  logMsg 0 ("Calc: " ++ s x ++ s y)

centerImg var surf canvas = do
  w1 <- imageSurfaceGetWidth surf
  h1 <- imageSurfaceGetHeight surf
  (w2,h2) <- widgetGetSizeRequest canvas
  let
    dh = intToDouble (h2 - h1)
    dw = intToDouble (w2 - w1)
  modifyMVar_ var (\_ -> return (1.0,dw / 2,dh / 2))

keyInput var surf canvas key = do
  liftIO $ print key
  case key of
    "q" -> do
      liftIO $ mainQuit
    "1" -> do
      liftIO $ centerImg var surf canvas

changeRef var d mouseX mouseY = do
  (scaleOld,oldX,oldY) <- readMVar var
  let
    scaleD = scale1 d
    scaleNew = scaleD * scaleOld
    dx = (mouseX - oldX) * (scaleD - 1)
    dy = (mouseY - oldY) * (scaleD - 1)
    newX = oldX - dx
    newY = oldY - dy
    result =  (scaleNew,newX,newY)
  modifyMVar_ var (\_ -> return result)
  logMsg 0 ("Change MVar: " ++ s scaleNew ++ 
    s newX ++ s newY)
  where
    factor = 5 / 4
    scale1 ScrollUp   = factor
    scale1 ScrollDown = 1 / factor

updateCanvas1 var canvas surf = do
  win <- widgetGetDrawWindow canvas
  (width, height) <- widgetGetSize canvas
  renderWithDrawable win $
    paintImage1 var surf
  return True

surfaceFromPNG file =
  withImageSurfaceFromPNG file $ \png -> do
    liftIO $ logMsg 0 "Load Image"
    w <- renderWith png $ imageSurfaceGetWidth png
    h <- renderWith png $ imageSurfaceGetHeight png
    surf <- createImageSurface FormatRGB24 w h
    renderWith surf $ do
      setSourceSurface png 0 0
      paint
    return surf

paintImage1 var surf = do
  (sc,x,y) <- liftIO $ readMVar var
  setColor white
  paint
  translate x y
  scale sc sc
  liftIO $ logMsg 0 ("Paint Image: " ++ 
    s sc ++ s x ++ s y)
  setSourceSurface surf 0 0
  paint

logMsg 0 s = do
  return ()
logMsg 1 s = do
  putStrLn s
  return ()
