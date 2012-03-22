{-# LANGUAGE PackageImports #-}

import "gloss-gtk" Graphics.Gloss
import qualified   Graphics.UI.Gtk as Gtk
import             Graphics.UI.Gtk (AttrOp((:=)))
import             Control.Monad.Trans

main = do
  _ <- Gtk.initGUI

  window <- Gtk.windowNew
  vbox   <- Gtk.vBoxNew False 0
  ebox   <- Gtk.eventBoxNew 
  btn    <- Gtk.buttonNewWithLabel "Press me"

  Gtk.boxPackStart vbox ebox Gtk.PackNatural 0
  Gtk.boxPackStart vbox btn Gtk.PackNatural 0

  Gtk.set window [ Gtk.containerBorderWidth := 0, Gtk.containerChild := vbox ]

  window `Gtk.on` Gtk.deleteEvent $ liftIO Gtk.mainQuit >> return False

  Gtk.widgetShowAll window
  drawPic ebox
  Gtk.mainGUI

drawPic w  = animate (InWidget w (800, 600))
                  white frame

frame time
        = Scale 0.8 0.8
        $ Rotate (time * 30)
        $ mach time 6
        
mach t 0 = leaf
mach t d
 = Pictures
        [ leaf
        , Translate 0 (-100) 
                $ Scale 0.8 0.8 
                $ Rotate (90 + t * 30) 
                $ mach (t * 1.5) (d - 1)

        , Translate 0   100 
                $ Scale 0.8 0.8 
                $ Rotate (90 - t * 30) 
                $ mach (t * 1.5) (d - 1) ]
        
leaf    = Pictures
                [ Color (makeColor 1.0 1.0 1.0 0.5) $ Polygon loop
                , Color (makeColor 0.0 0.0 1.0 0.8) $ Line loop ]

loop    = [(-10, -100), (-10, 100), (10, 100), (10, -100), (-10, -100)]
