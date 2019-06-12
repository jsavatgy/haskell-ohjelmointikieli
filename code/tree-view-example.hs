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


