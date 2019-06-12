
import Control.Concurrent.MVar 
import System.IO.Unsafe
import Graphics.UI.Gtk 
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.UI.Gtk.Gdk.EventM as M
import System.Glib.UTFString (glibToString)
import Eemian

main = do
  initGUI
  var <- newMVar (1.0,0.0,0.0)
  vPos <- newMVar (None,0.0,0.0)
  window <- windowNew
  canvas <- drawingAreaNew
  surf <- return $ unsafeLoadPNG "korkalo-1.png"
  widgetAddEvents canvas [Button1MotionMask] 
  widgetSetSizeRequest canvas 600 600
  centerImg var surf canvas 
  canvas `on` motionNotifyEvent $ do
    (mouseX,mouseY) <- eventCoordinates
    t <- M.eventTime
    C.liftIO $ changePos vPos var surf canvas mouseX mouseY
    C.liftIO $ logMsg 0 ("Motion Time: " ++ s t)
    return False
  window `on` keyPressEvent $ tryEvent $ do
    key <- eventKeyName
    keyInput var surf canvas (glibToString key)
    C.liftIO $ updateCanvas1 var canvas surf
    return ()
  canvas `on` buttonPressEvent $ tryEvent $ do
    (mouseX,mouseY) <- printMouse
    C.liftIO $ printPointer canvas
    C.liftIO $ printMVar var mouseX mouseY
    C.liftIO $ modifyMVar_ vPos (\_ -> return (Press,mouseX,mouseY)) 
  canvas `on` buttonReleaseEvent $ tryEvent $ do
    (mouseX,mouseY) <- M.eventCoordinates
    m <- M.eventModifier
    b <- M.eventButton
    (cause,vPosX,vPosY) <- C.liftIO $ readMVar vPos
    C.liftIO $ release cause b var vPosX vPosY
  canvas `on` scrollEvent $ tryEvent $ do
    (mouseX,mouseY) <- M.eventCoordinates
    m <- M.eventModifier
    d <- M.eventScrollDirection
    t <- M.eventTime
    C.liftIO $ changeRef var d mouseX mouseY
    C.liftIO $ updateCanvas1 var canvas surf
    C.liftIO $ logMsg 0 ("Scroll: " ++ s t ++ s mouseX ++
      s mouseY ++ s m ++ s d) 
  onDestroy window mainQuit
  onExpose canvas $ const (updateCanvas1 var canvas surf)
  set window [containerChild := canvas]
  widgetShowAll window
  mainGUI

data EvtType = Press | Release | Move | Scroll | None

release Press button var mouseX mouseY = do
  (varS,varX,varY) <- readMVar var
  let
    x = (mouseX - varX) / varS
    y = (mouseY - varY) / varS
  C.liftIO $ logMsg 0 ("Add point: " ++ s x ++ s y ++ s button)
  C.liftIO $ logMsg 1 (s x ++ s y)

release _ button var x y = do
  C.liftIO $ logMsg 0 ("Ignore: " ++ s x ++ s y)

changePos vPos var surf canvas mouseX mouseY = do
  (cause,vPosX,vPosY) <- readMVar vPos
  (scaleOld,oldX,oldY) <- readMVar var
  let
    dx = vPosX - mouseX
    dy = vPosY - mouseY
  modifyMVar_ var (\_ -> return (scaleOld,oldX-dx,oldY-dy))
  modifyMVar_ vPos (\_ -> return (Move,mouseX,mouseY))
  updateCanvas1 var canvas surf

s x = show x ++ " "

printMouse = do
  (mouseX,mouseY) <- M.eventCoordinates
  C.liftIO $ logMsg 0 ("Mouse: " ++ s mouseX ++ s mouseY)
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
  w1 <- C.imageSurfaceGetWidth surf
  h1 <- C.imageSurfaceGetHeight surf
  (w2,h2) <- widgetGetSizeRequest canvas
  let
    dh = intToDouble (h2 - h1)
    dw = intToDouble (w2 - w1)
  modifyMVar_ var (\_ -> return (1.0,dw/2,dh/2))

keyInput var surf canvas key = do
  C.liftIO $ print key
  case key of
    "q" -> do
      C.liftIO $ mainQuit
    "1" -> do
      C.liftIO $ centerImg var surf canvas

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
    factor = 5/4
    scale1 ScrollUp   = factor
    scale1 ScrollDown = 1/factor

updateCanvas1 var canvas surf = do
  win <- widgetGetDrawWindow canvas
  (width, height) <- widgetGetSize canvas
  renderWithDrawable win $
    paintImage1 var surf
  return True

imageSurfaceCreateFromPNG :: FilePath -> IO C.Surface
imageSurfaceCreateFromPNG file =
  C.withImageSurfaceFromPNG file $ \png -> do
    C.liftIO $ logMsg 0 "Load Image"
    w <- C.renderWith png $ C.imageSurfaceGetWidth png
    h <- C.renderWith png $ C.imageSurfaceGetHeight png
    surf <- C.createImageSurface C.FormatRGB24 w h
    C.renderWith surf $ do
      C.setSourceSurface png 0 0
      C.paint
    return surf

unsafeLoadPNG file = unsafePerformIO $ imageSurfaceCreateFromPNG file

paintImage1 var surf = do
  (sc,x,y) <- C.liftIO $ readMVar var
  C.setSourceRGB 1 1 1
  C.paint
  C.translate x y
  C.scale sc sc
  C.liftIO $ logMsg 0 ("Paint Image: " ++ s sc ++ s x ++ s y)
  C.setSourceSurface surf 0 0
  C.paint

logMsg 0 s = do
  return ()
logMsg 1 s = do
  putStrLn s
  return ()

