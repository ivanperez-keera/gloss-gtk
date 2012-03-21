-- GTK Backend for gloss
{-# OPTIONS_HADDOCK hide #-}
module Graphics.Gloss.Internals.Interface.Backend.GtkGL
        (GtkGLState)
where

import           Data.IORef
import           Data.Maybe
import           Control.Monad
import           Control.Concurrent
import           Graphics.Gloss.Internals.Interface.Backend.Types
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL                        as GL
import qualified Graphics.UI.GLUTGtk                              as GLUTGtk       -- (glut, Size(Size), widget)
import           Graphics.UI.Gtk                                  hiding (Display) -- (containerAdd)
import qualified Graphics.UI.Gtk                                  as Gtk
import           Graphics.UI.Gtk.OpenGL
import qualified System.Exit                                      as System
import           System.Posix.Clock


-- | We Need to keep a reference to the gl widget,
--   we need to know wether we have our own window or
--   not, and we need to keep track of the time when
--   the initialisation took place.
data GtkGLState = GtkGLState
  { glGtk      :: (Maybe GLUTGtk.GLUTGtk)   -- gl widget, if any
  , glWindowed :: Bool              -- whether gl is the top widget or not
  , glInitTime :: (Maybe TimeSpec)  -- when initialised was first called
  }

-- | Initial state: no widget, no window, no time
gtkStateInit :: GtkGLState
gtkStateInit  = GtkGLState Nothing False Nothing

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

        installKeyMouseCallback    = installKeyMouseCallbackGLUT

        -- TODO: iperez: To be completed
        installMotionCallback      = \_ _ -> return ()  -- installMotionCallbackGLUT

        installIdleCallback        = installIdleCallbackGtkGL

        -- Call the main gui
        -- This function will return when something calls leaveMainLoop
        runMainLoop ref 
         = do (GtkGLState mgl windowed _) <- readIORef ref
              when (windowed && isJust mgl) mainGUI

        postRedisplay ref
         = do (GtkGLState mgl _ _) <- readIORef ref
              maybe (return ()) GLUTGtk.postRedisplay mgl

        getWindowDimensions ref 
         = do (GtkGLState mgl windowed _) <- readIORef ref
              maybe (return (0,0)) (Gtk.widgetGetSize . GLUTGtk.widget) mgl
              -- putStrLn.("new size" ++).show =<< x

        elapsedTime ref
         = do (GtkGLState _ _ mts) <- readIORef ref

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

-- Initialise -----------------------------------------------------------------
initializeGtkGL
        :: IORef GtkGLState
        -> Bool
        -> IO ()

initializeGtkGL ref debug = do
     -- We don't know whether these were called from somewhere else, so we have
     -- to do it here instead.
     _ <- initGL  -- we ignore gl args
     _ <- initGUI -- we ignore gtk args

     -- Save initial time
     ts <- getTime Monotonic
     modifyIORef ref (\st -> st { glInitTime = Just ts })

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
               eventb <- eventBoxNew 
               gl     <- GLUTGtk.glut eventb (GLUTGtk.Size sizeX sizeY)
               modifyIORef (GLUTGtk.realizeCallback gl) $ const $
                 GL.drawBuffer $= GL.BackBuffers
                 -- glDrawableSwapBuffers

               Gtk.set w [ Gtk.containerBorderWidth := 0
                         , Gtk.containerChild := eventb
                         ]
               Gtk.windowMove w posX posY
               Gtk.widgetShowAll w

               -- Update state
               modifyIORef ref (\st -> st { glGtk = Just gl , glWindowed = True })

          FullScreen _ -> error "Cannot go fullscreen in Gtk"

          InWidget bin (sizeX, sizeY) ->
            do gl <- GLUTGtk.glut bin (GLUTGtk.Size sizeX sizeY)
               widgetShowAll (GLUTGtk.widget gl)
               modifyIORef (GLUTGtk.realizeCallback gl) $ const $
                 GL.drawBuffer $= GL.BackBuffers
               modifyIORef ref (\st -> st { glGtk      = Just gl
                                          , glWindowed = False })

-- Dump State -----------------------------------------------------------------
dumpStateGtkGL
        :: IORef GtkGLState
        -> IO ()

dumpStateGtkGL st = do
 (GtkGLState mgl w mts) <- readIORef st
 putStrLn $ " Gtk status report: widget created: " ++ show (isJust mgl)
          ++ ", windowed: " ++ show w
          ++ ", time set: " ++ show (isJust mts)

-- Display Callback -----------------------------------------------------------
installDisplayCallbackGtkGL
        :: IORef GtkGLState -> [Callback]
        -> IO ()
installDisplayCallbackGtkGL ref callbacks
        = do (GtkGLState mgl _ _) <- readIORef ref
             flip (maybe (return ())) mgl $ \gl ->
               modifyIORef (GLUTGtk.displayCallback gl) $ \_ -> 
                                     (callbackDisplay ref callbacks)
 
callbackDisplay 
        :: IORef GtkGLState -> [Callback]
        -> IO ()

callbackDisplay ref callbacks 
 = do (GtkGLState mgl _ _) <- readIORef ref
      case mgl of
       Nothing -> error "Trying to display a non-existing gl widget"
       Just gl -> do
        -- clear the display
        GL.clear [GL.ColorBuffer] -- , GL.DepthBuffer
        -- GL.clearColor $= (GL.Color4 1.0 0.0 0.0 (0.5 :: GL.GLclampf))

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
        = do (GtkGLState mgl _ _) <- readIORef ref
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
-- FIXME: iperez: ignore for now
installKeyMouseCallbackGLUT 
        :: IORef GtkGLState -> [Callback]
        -> IO ()

installKeyMouseCallbackGLUT ref callbacks
        = do (GtkGLState mgl _ _) <- readIORef ref
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
  = sequence_
  $ map (\f -> f key' keyState' modifiers' pos)
        [f ref | KeyMouse f <- callbacks]
   where
     key'       = gtkKeyToKey key
     keyState'  = gtkKeyStateToKeyState keystate
     modifiers' = gtkModifiersToModifiers modifiers
     pos        = maybe (0,0) (\(GLUTGtk.Position x y) -> (fromEnum x, fromEnum y)) mpos

gtkKeyToKey             = undefined

-- -- Motion Callback ------------------------------------------------------------
-- installMotionCallbackGLUT 
--         :: IORef GLUTState -> [Callback]
--         -> IO ()
-- 
-- installMotionCallbackGLUT ref callbacks
--  = do   GLUT.motionCallback        $= Just (callbackMotion ref callbacks)
--         GLUT.passiveMotionCallback $= Just (callbackMotion ref callbacks)
-- 
-- callbackMotion
--         :: IORef GLUTState -> [Callback]
--         -> GLUT.Position
--         -> IO ()
-- 
-- callbackMotion ref callbacks (GLUT.Position posX posY)
--  = do   let pos = (fromEnum posX, fromEnum posY)
--         sequence_
--          $ map  (\f -> f pos)
--                 [f ref | Motion f <- callbacks]


-- Idle Callback --------------------------------------------------------------
installIdleCallbackGtkGL
        :: IORef GtkGLState -> [Callback]
        -> IO ()

installIdleCallbackGtkGL ref callbacks = do
  widgetExists <- fmap (isJust.glGtk) $ readIORef ref
  when widgetExists $ void $
    Gtk.idleAdd callback priorityDefaultIdle

  where callback = callbackIdle ref callbacks >> return True

callbackIdle 
        :: IORef GtkGLState -> [Callback]
        -> IO ()

callbackIdle ref callbacks
        = sequence_ [ f ref | (Idle f) <- callbacks]

-------------------------------------------------------------------------------
---- | Convert GLUTs key codes to our internal ones.
--gtkKeyToKey :: String -> Key
--gtkKeyToKey key 
-- = case key of
--        "\32"                            -> SpecialKey KeySpace
--        "\13"                            -> SpecialKey KeyEnter
--        "\9"                             -> SpecialKey KeyTab
--        "\ESC"                           -> SpecialKey KeyEsc
--        "\DEL"                           -> SpecialKey KeyDelete
--        [c]                              -> Char c
        -- MouseButton GLUT.LeftButton           -> MouseButton LeftButton
        -- MouseButton GLUT.MiddleButton         -> MouseButton MiddleButton
        -- MouseButton GLUT.RightButton          -> MouseButton RightButton
        -- MouseButton GLUT.WheelUp              -> MouseButton WheelUp
        -- MouseButton GLUT.WheelDown            -> MouseButton WheelDown
        -- MouseButton (GLUT.AdditionalButton i) -> MouseButton (AdditionalButton i)

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
