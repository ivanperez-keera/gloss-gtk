{-# LANGUAGE PackageImports #-}

import "gloss-gtk" Graphics.Gloss
import "gloss-gtk" Graphics.Gloss.Interface.IO.Game
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


drawPic ebox = let state = State Nothing []
  in play (InWidget ebox (600, 600))
          white 100 state
          makePicture handleEvent stepWorld

-- | The game state.
data State      
        = State (Maybe Path)    -- The current line being drawn.
                [Picture]       -- All the lines drawn previously.


-- | A Line Segment
type Segment    = ((Float, Float), (Float, Float))


-- | Convert our state to a picture.
makePicture :: State -> Picture
makePicture (State m xs)
        = Pictures (maybe xs (\x -> Line x : xs) m)


-- | Handle mouse click and motion events.
handleEvent :: Event -> State -> State
handleEvent event state
        -- If the mouse has moved, then extend the current line.
        | EventMotion (x, y)    <- event
        , State (Just ps) ss    <- state
        = State (Just ((x, y):ps)) ss 

        -- Start drawing a new line.
        | EventKey (MouseButton LeftButton) Down _ pt@(x,y) <- event
        , State Nothing ss       <- state
        = State (Just [pt])
                ((Translate x y $ Scale 0.1 0.1 $ Color violet $ Text "Down") : ss)

        -- Finish drawing a line, and add it to the picture.
        | EventKey (MouseButton LeftButton) Up _ pt@(x,y)      <- event
        , State (Just ps) ss    <- state
        = State Nothing
                ((Translate x y $ Scale 0.1 0.1 $ Text "up") : Line (pt:ps) : ss)

        | otherwise
        = state


stepWorld :: Float -> State -> State
stepWorld _ = id

