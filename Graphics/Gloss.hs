
-- | Gloss hides the pain of drawing simple vector graphics behind a nice data type and
--	a few display functions. 
--
--   Getting something on the screen is as easy as:
--
--  @
--    import Graphics.Gloss
--    main = `display` (InWindow \"Nice Window\" (200, 200) (10, 10)) `white` (`Circle` 80)
--  @
--
--   Once the window is open you can use the following:
--
-- 	* Quit - esc-key.
--
--	* Move Viewport - left-click drag, arrow keys.
--
--	* Rotate Viewport - right-click drag, control-left-click drag, or home\/end-keys.
--
--	* Zoom Viewport - mouse wheel, or page up\/down-keys.
--
--   Animations can be constructed similarly using the `animate`.
--
--   If you want to run a simulation based around finite time steps then try
--   `simulate`.
--
--   If you want to manage your own key\/mouse events then use `play`.
-- 
--   Gloss uses OpenGL under the hood, but you don't have to worry about any of that.
--
--   Gloss programs should be compiled with @-threaded@, otherwise the GHC runtime
--   will limit the frame-rate to around 20Hz.
--
--
-- @Release Notes:
--
-- For 1.7.0:
--   * Tweaked circle level-of-detail reduction code.
--   * Increased frame rate cap to 100hz.
--   Thanks to Doug Burke
--   * Primitives for drawing arcs and sectors.
--   Thanks to Thomas DuBuisson
--   * IO versions of animate, simplate and play.
--
-- For 1.6.0:
--   Thanks to Anthony Cowley
--   * Full screen display mode.
-- 
-- For 1.5.0:
--   * O(1) Conversion of ForeignPtrs to bitmaps.
--   * An extra flag on the Bitmap constructor allows bitmaps to be cached
--     in texture memory between frames.
--
-- For 1.4.0:
--   Thanks to Christiaan Baaij: 
--   * Refactoring of Gloss internals to support multiple window manager backends.
--   * Support for using GLFW as the window library instead of GLUT.
--     GLUT is still the default, but to use GLFW install gloss with:
--        cabal install gloss --flags=\"GLFW -GLUT\"
-- @
--
-- For more information, check out <http://gloss.ouroborus.net>.
--
module Graphics.Gloss 
	( module Graphics.Gloss.Data.Picture
	, module Graphics.Gloss.Data.Color
        , Display(..)
	, display
	, animate
        , simulate
	, play)
where
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.Pure.Display
import Graphics.Gloss.Interface.Pure.Animate
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Game
