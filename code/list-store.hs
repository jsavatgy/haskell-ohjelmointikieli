import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as Model
import Control.Monad.Trans (liftIO)
import Text.Regex.Posix
import Control.Exception

main = do
  content <- readFile "moon-latin.txt"
  let moon = filter (not . null) (lines content)
  initGUI
  window <- windowNew
  vbox1 <- vBoxNew False 0
  entry <- entryNew     
  containerAdd window vbox1
  listore <- listStoreNew []
  treeview <- Model.treeViewNewWithModel listore
  entry `on` keyReleaseEvent $ do
    liftIO $ updateList1 entry listore moon
  Model.treeViewSetHeadersVisible treeview False
  col <- Model.treeViewColumnNew
  renderer <- Model.cellRendererTextNew
  Model.cellLayoutPackStart col renderer False
  Model.cellLayoutSetAttributes col renderer listore
    (\text -> [Model.cellText := text])
  Model.treeViewAppendColumn treeview col
  tree <- Model.treeViewGetSelection treeview
  boxPackStart vbox1 entry PackNatural 0
  boxPackStart vbox1 treeview PackNatural 0
  widgetShowAll window
  liftIO $ updateList1 entry listore moon
  onDestroy window mainQuit
  mainGUI

tryFilter :: String -> [String] 
          -> IO (Either SomeException [String])
tryFilter txt list = do 
  result <- try (evaluate (filter (=~ txt) list)) 
  return result

filter1 txt list = do
  result <- tryFilter txt list
  return $ case result of
    Left ex -> []
    Right val -> val

updateList1 entry listore list = do
  listStoreClear listore
  txt <- entryGetText entry
  list1 <- listStoreToList listore
  list2 <- filter1 txt list
  let
    n = 8
    list3 = take (n + 1) list2
    l2 = length list2 
    l3 = length list3
    list4 
      | l2 == l3 = take (n + 1) (list2 ++ repeat "")
      | l2 > l3  = take n list3 ++ 
        ["(+" ++ show (l2 - l3 + 1) ++ " others)"]
  mapM_ (listStoreAppend listore) list4
  return True
