-- GTK Backend for gloss
{-# OPTIONS_HADDOCK hide #-}
module Graphics.Gloss.Internals.Interface.Backend.GtkGL
        (GtkGLState)
where

import           Data.Char (toLower)
import           Data.IORef
import           Data.Maybe
import           Control.Monad
import           Control.Concurrent
import           Graphics.Gloss.Internals.Interface.Backend.Types
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL                        as GL
import qualified Graphics.UI.GLUTGtk                              as GLUTGtk       -- (glut, Size(Size), widget)
-- import           Graphics.UI.Gtk                                  hiding (Display) -- (containerAdd)
import           Graphics.UI.Gtk                                  (AttrOp((:=)))
import qualified Graphics.UI.Gtk                                  as Gtk
import           Graphics.UI.Gtk.OpenGL
import qualified System.Exit                                      as System
import           System.Posix.Clock
import           Graphics.Rendering.FTGL
import           Paths_gloss_gtk


-- | We Need to keep a reference to the gl widget,
--   we need to know wether we have our own window or
--   not, and we need to keep track of the time when
--   the initialisation took place.
data GtkGLState = GtkGLState
  { glGtk      :: Maybe GLUTGtk.GLUTGtk   -- gl widget, if any
  , glWindowed :: Bool            -- whether gl is the top widget or not
  , glInitTime :: Maybe TimeSpec  -- when initialised was first called
  , glFont     :: Maybe Font
  }

-- | Initial state: no widget, no window, no time
gtkStateInit :: GtkGLState
gtkStateInit  = GtkGLState Nothing False Nothing Nothing

instance Backend GtkGLState where
        initBackendState           = gtkStateInit
        initializeBackend          = initializeGtkGL

        -- non-freeglut doesn't like this: (\_ -> GLUT.leaveMainLoop)
        -- TODO: iperez: Do we really want to allow users to exit
        -- anything from inside glut? Do we want to allow them
        -- to exit the application? Maybe we could just notify
        -- gtk that gloss stopped somehow.
        exitBackend                = (\_ -> System.exitWith System.ExitSuccess)

        openWindow                 = openWindowGtkGL
        dumpBackendState           = dumpStateGtkGL
        installDisplayCallback     = installDisplayCallbackGtkGL

        -- TODO: iperez: Try calling Gtk's leaveMainLoop from here
        installWindowCloseCallback = (\_ -> return ())

        installReshapeCallback     = installReshapeCallbackGtkGL
        installKeyMouseCallback    = installKeyMouseCallbackGtkGL
        installMotionCallback      = installMotionCallbackGtkGL
        installIdleCallback        = installIdleCallbackGtkGL

        -- Call the main gui
        -- This function will return when something calls leaveMainLoop
        runMainLoop ref 
         = do (GtkGLState mgl windowed _ _) <- readIORef ref
              when (windowed && isJust mgl) Gtk.mainGUI

        postRedisplay ref
         = do (GtkGLState mgl _ _ _) <- readIORef ref
              maybe (return ()) GLUTGtk.postRedisplay mgl

        getWindowDimensions ref 
         = do (GtkGLState mgl windowed _ _) <- readIORef ref
              maybe (return (0,0)) getContainerSize mgl
              -- putStrLn.("new size" ++).show =<< x
              -- x
          where getContainerSize (GLUTGtk.GLUTGtk { GLUTGtk.widget = w }) = Gtk.widgetGetSize w

        elapsedTime ref
         = do (GtkGLState _ _ mts _) <- readIORef ref

              -- TODO: iperez: check if there's a better, maybe even faster
              -- time library, since do not need the precision provided by this one

              t <- getTime Realtime
              let t1 = toMicro (sec t) (nsec t)
                  t2 = maybe t1 (\ts -> toMicro (sec ts) (nsec ts)) mts
                  toMicro s n = (fromIntegral s) + (fromIntegral n / 1000000000)
              return (t1 - t2)

        sleep _ sec
         = do -- putStrLn $ "Sleeping for: " ++ show sec
              threadDelay (round $ sec * 100000)

        font ref
         = fmap glFont $ readIORef ref

-- Initialise -----------------------------------------------------------------
initializeGtkGL
        :: IORef GtkGLState
        -> Bool
        -> IO ()

initializeGtkGL ref debug = do
     -- We don't know whether these were called from somewhere else, so we have
     -- to do it here instead.
     _ <- initGL  -- we ignore gl args
     _ <- Gtk.initGUI -- we ignore gtk args

     -- Save initial time
     ts <- getTime Realtime

     fn <- getDataFileName "data/FreeSans.ttf"
     font <- createTextureFont fn
     setFontFaceSize font 224 72

     modifyIORef ref (\st -> st { glInitTime = Just ts, glFont = Just font })

-- Open Window ----------------------------------------------------------------
openWindowGtkGL
        :: IORef GtkGLState
        -> Display
        -> IO ()

openWindowGtkGL ref display
 = do
       -- Setup and create gl widget (and possibly, a window for it). Window
       -- positions are ignored.
        case display of
          InWindow windowName (sizeX, sizeY) (posX, posY) -> 
            do w      <- Gtk.windowNew 
               gl     <- GLUTGtk.glut w (GLUTGtk.Size sizeX sizeY)
               modifyIORef (GLUTGtk.realizeCallback gl) $ const $
                 GL.drawBuffer $= GL.BackBuffers
                 -- glDrawableSwapBuffers

               Gtk.set w [ Gtk.containerBorderWidth := 0]
               Gtk.windowMove w posX posY
               Gtk.widgetShowAll w

               -- Update state
               modifyIORef ref (\st -> st { glGtk = Just gl , glWindowed = True })

          FullScreen _ -> error "Cannot go fullscreen in Gtk"

          InWidget bin (sizeX, sizeY) ->
            do gl <- GLUTGtk.glut bin (GLUTGtk.Size sizeX sizeY)

               -- We need this aux function to extract the widget from the
               -- GLUTGtk value
               let containerShowAll (GLUTGtk.GLUTGtk { GLUTGtk.widget = w }) =
                      Gtk.widgetShowAll w

               containerShowAll gl
               modifyIORef (GLUTGtk.realizeCallback gl) $ const $
                 GL.drawBuffer $= GL.BackBuffers
               modifyIORef ref (\st -> st { glGtk      = Just gl
                                          , glWindowed = False })

-- Dump State -----------------------------------------------------------------
dumpStateGtkGL
        :: IORef GtkGLState
        -> IO ()

dumpStateGtkGL st = do
 (GtkGLState mgl w mts _) <- readIORef st
 putStrLn $ " Gtk status report: widget created: " ++ show (isJust mgl)
          ++ ", windowed: " ++ show w
          ++ ", time set: " ++ show (isJust mts)

-- Display Callback -----------------------------------------------------------
installDisplayCallbackGtkGL
        :: IORef GtkGLState -> [Callback]
        -> IO ()
installDisplayCallbackGtkGL ref callbacks
        = do (GtkGLState mgl _ _ _) <- readIORef ref
             flip (maybe (return ())) mgl $ \gl ->
               modifyIORef (GLUTGtk.displayCallback gl) $ \_ -> 
                                     (callbackDisplay ref callbacks)
 
callbackDisplay 
        :: IORef GtkGLState -> [Callback]
        -> IO ()

callbackDisplay ref callbacks 
 = do (GtkGLState mgl _ _ _) <- readIORef ref
      case mgl of
       Nothing -> error "Trying to display a non-existing gl widget"
       Just gl -> do

        -- clear the display
        GL.clear [GL.ColorBuffer]
        GL.clearColor $= (GL.Color4 1.0 1.0 1.0 (1.0 :: GL.GLclampf))
        GL.color $ GL.Color4 0 0 0 (1 :: GL.GLfloat)

        -- get the display callbacks from the chain and run them
        sequence_ [f ref | (Display f) <- callbacks]

        -- swap front and back buffers
        -- GLUTGtk.postRedisplay gl
        -- swapBuffers
        -- GLUT.swapBuffers
        -- GLUT.reportErrors

-- Reshape Callback -----------------------------------------------------------
installReshapeCallbackGtkGL
        :: IORef GtkGLState -> [Callback]
        -> IO ()

installReshapeCallbackGtkGL ref callbacks
        = do (GtkGLState mgl _ _ _) <- readIORef ref
             flip (maybe (return ())) mgl $ \gl ->
               modifyIORef (GLUTGtk.reshapeCallback gl) $ const $
                               (callbackReshape ref callbacks)

callbackReshape
        :: IORef GtkGLState -> [Callback]
        -> GLUTGtk.Size
        -> IO ()

callbackReshape ref callbacks (GLUTGtk.Size w h)
        = let sz = (fromEnum w, fromEnum h)
          in -- iperez: these do not seem to be necessary
             -- GL.matrixMode $= GL.Projection
             -- GL.loadIdentity
             -- GL.ortho 45.0 (fromIntegral w) 45.0 (fromIntegral h) (1.0) 1.0
             sequence_ [f ref sz | Reshape f <- callbacks]

-- KeyMouse Callback ----------------------------------------------------------
installKeyMouseCallbackGtkGL
        :: IORef GtkGLState -> [Callback]
        -> IO ()

installKeyMouseCallbackGtkGL ref callbacks
        = do (GtkGLState mgl _ _ _) <- readIORef ref
             flip (maybe (return ())) mgl $ \gl ->
               modifyIORef (GLUTGtk.keyboardMouseCallback gl) $ const $
                               (callbackKeyMouse ref callbacks)

callbackKeyMouse
        :: IORef GtkGLState -> [Callback]
        -> GLUTGtk.Key
        -> GLUTGtk.KeyState
        -> [Gtk.Modifier]
        -> Maybe GLUTGtk.Position
        -> IO ()

callbackKeyMouse ref callbacks key keystate modifiers mpos
 | isNothing key'
 = return ()

 | otherwise
 = sequence_ $
     map (\f -> f (fromJust key') keyState' modifiers' pos') 
       [f ref | KeyMouse f <- callbacks]
   where
     key'       = gtkKeyToKey key
     keyState'  = gtkKeyStateToKeyState keystate
     modifiers' = gtkModifiersToModifiers modifiers
     pos'       = maybe (0,0) (\(GLUTGtk.Position x y) -> (round x, round y)) mpos

-- Motion Callback ------------------------------------------------------------
installMotionCallbackGtkGL
        :: IORef GtkGLState -> [Callback]
        -> IO ()

installMotionCallbackGtkGL ref callbacks
 = do (GtkGLState mgl _ _ _) <- readIORef ref
      flip (maybe (return ())) mgl $ \gl ->
        modifyIORef (GLUTGtk.mouseMoveCallback gl) $ const $
          (callbackMotion ref callbacks)

callbackMotion
        :: IORef GtkGLState -> [Callback]
        -> GLUTGtk.Position
        -> IO ()

callbackMotion ref callbacks (GLUTGtk.Position posX posY)
 = let pos = (fromEnum posX, fromEnum posY)
   in do 
      sequence_
        $ map (\f -> f pos) l
   where l = [f ref | Motion f <- callbacks]


-- Idle Callback --------------------------------------------------------------
installIdleCallbackGtkGL
        :: IORef GtkGLState -> [Callback]
        -> IO ()

installIdleCallbackGtkGL ref callbacks = do
  widgetExists <- fmap (isJust.glGtk) $ readIORef ref
  when widgetExists $ void $
    Gtk.idleAdd callback Gtk.priorityDefaultIdle

  where callback = callbackIdle ref callbacks >> return True

callbackIdle 
        :: IORef GtkGLState -> [Callback]
        -> IO ()

callbackIdle ref callbacks
        = sequence_ [ f ref | (Idle f) <- callbacks]

-----------------------------------------------------------------------------
-- | Convert GLUTGtk's key codes to our internal ones.
gtkKeyToKey :: GLUTGtk.Key-> Maybe Key
gtkKeyToKey (GLUTGtk.Key key) =
  case key of
    "Space"  -> Just $ SpecialKey KeySpace
    "Return" -> Just $ SpecialKey KeyEnter
    "Tab"    -> Just $ SpecialKey KeyTab
    "Escape" -> Just $ SpecialKey KeyEsc
    "Delete" -> Just $ SpecialKey KeyDelete
    [x]      -> Just $ Char (toLower x)
    _        -> Nothing

gtkKeyToKey (GLUTGtk.MouseButton btn) =
  case btn of
    GLUTGtk.LeftButton    -> Just $ MouseButton LeftButton
    GLUTGtk.MiddleButton  -> Just $ MouseButton MiddleButton
    GLUTGtk.RightButton   -> Just $ MouseButton RightButton
    GLUTGtk.OtherButton i -> Just $ MouseButton (AdditionalButton i)

gtkKeyToKey (GLUTGtk.MouseScroll dir) =
  case dir of
    GLUTGtk.ScrollUp   -> Just $ MouseButton WheelUp
    GLUTGtk.ScrollDown -> Just $ MouseButton WheelDown
    _                  -> Nothing

-- | Convert GLUTGtk's key state to our internal ones.
gtkKeyStateToKeyState :: GLUTGtk.KeyState -> KeyState
gtkKeyStateToKeyState GLUTGtk.Down = Down
gtkKeyStateToKeyState GLUTGtk.Up   = Up

-- | Convert GLUTs key states to our internal ones.
gtkModifiersToModifiers 
        :: [GLUTGtk.Modifier]
        -> Modifiers
        
gtkModifiersToModifiers mods
        = Modifiers shiftPressed ctrlPressed altPressed

 where shiftPressed = if (GLUTGtk.Shift   `elem` mods) then Down else Up
       ctrlPressed  = if (GLUTGtk.Control `elem` mods) then Down else Up
       altPressed   = if (GLUTGtk.Alt     `elem` mods) then Down else Up
