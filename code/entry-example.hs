import Graphics.UI.Gtk

main = do
  initGUI
  window <- windowNew
  entry <- entryNew     
  containerAdd window entry
  widgetShowAll window 
  onDestroy window mainQuit
  mainGUI

