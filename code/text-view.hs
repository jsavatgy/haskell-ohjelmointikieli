
import Graphics.UI.Gtk

main = do
  initGUI
  content <- readFile "puulajit-latina.txt"
  window <- windowNew
  sw <- scrolledWindowNew Nothing Nothing
  set sw [
    scrolledWindowVscrollbarPolicy := PolicyAlways,
    scrolledWindowHscrollbarPolicy := PolicyAutomatic ]
  textView <- textViewNew
  buffer <- textViewGetBuffer textView
  font <- fontDescriptionFromString "Monospace 9"
  widgetModifyFont textView (Just font)
  widgetModifyBase textView StateNormal (gray 0.94)
  textBufferSetText buffer content
  containerAdd (toContainer sw) textView
  set window [
    windowDefaultWidth := 310,
    windowDefaultHeight := 160,
    containerChild := sw]
  on window objectDestroy mainQuit
  widgetShowAll window
  mainGUI

peachpuff = color 1.0 0.855 0.725
gray n = color n n n

color r g b = Color r1 g1 b1
  where
    [r1,g1,b1] = map (round . (*65535)) [r,g,b]

