module KCollMainGUI where
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

main :: FilePath -> IO ()
main fpath = do putStrLn $ "Will read ui from " ++ fpath
                initGUI
                Just xml <- xmlNew "glade-kcollgui.glade"
                window <- xmlGetWidget xml castToWindow "window1"
                widgetShowAll window
                window `onDestroy` mainQuit
                mainGUI
                putStrLn "done"
 
