{-# LANGUAGE ExistentialQuantification #-}

module Graphics.Gloss.Data.Display
        (Display(..))
where

import Graphics.UI.Gtk (EventBox)

-- | Describes how Gloss should display its output.
data Display 
        -- | Display in a window with the given name, size and position.
        = InWindow   String (Int, Int) (Int, Int)

        -- | Display full screen with a drawing area of the given size.
        | FullScreen (Int, Int) 

        -- | Display inside a GTK eventbox using an opengl widget
        | InWidget EventBox (Int, Int)

instance Show Display where

 show (InWindow name size pos) =
    "InWindow " ++ show name ++ " " ++ show size ++ " " ++ show pos
 show (FullScreen size) =
    "FullScreen " ++ show size
 show (InWidget _w size) =
    "InWidget _ " ++ show size

instance Eq Display where

 (InWindow n1 s1 p1) == (InWindow n2 s2 p2) = (n1,s1,p1) == (n2,s2,p2)
 (FullScreen s1) == (FullScreen s2) = s1 == s2
 _ == _ = False
