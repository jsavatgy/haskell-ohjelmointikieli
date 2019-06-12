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
