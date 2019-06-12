import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

main :: IO ()
main= do
     initGUI
     window <- windowNew
     set window [
       windowTitle := "Hour Glass", 
       windowDefaultWidth := 300, 
       windowDefaultHeight := 200,
       containerBorderWidth := 10 ]

     frame <- frameNew
     containerAdd window frame
     canvas <- drawingAreaNew
     containerAdd frame canvas

     widgetShowAll window 
     onExpose canvas $ const (updateCanvas1 canvas)
     onDestroy window mainQuit
     mainGUI

updateCanvas1 canvas = do
  win <- widgetGetDrawWindow canvas
  renderWithDrawable win myDraw
  return True

myDraw = do
    setSourceRGB 1 1 1
    paint

    setSourceRGB 0 0 0
    setLineWidth 5

    moveTo 120 60
    lineTo 60 110
    lineTo 180 110
    closePath

    stroke
