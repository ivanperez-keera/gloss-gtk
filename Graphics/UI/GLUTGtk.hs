-- | FIXME: iperez: This code was taken from pastebin. Although it is publicly
-- available, its redistribution can present a violation of the copyright law.
-- It should be substituted by our own code or simply removed.
module Graphics.UI.GLUTGtk
   ( module Graphics.UI.GLUTGtk
   , KeyVal, MouseButton, Modifier(..)
   )
  where

import           Control.Monad             (join)
import           Control.Monad.Trans       (liftIO)
import           Data.IORef                (IORef, newIORef, readIORef)
import           Graphics.UI.Gtk           hiding (Size, get)
import           Graphics.UI.Gtk.OpenGL

type RealizeCallback = IO ()
type ReshapeCallback = Size -> IO ()
type DisplayCallback = IO ()
type KeyboardMouseCallback = Key -> KeyState -> [Modifier] -> (Maybe Position) -> IO ()

data Size = Size Int Int
  deriving (Eq, Ord, Show)

data Position = Position Double Double
  deriving (Eq, Ord, Show)

data KeyState = Down | Up
  deriving (Eq, Ord, Show)

data Key = Key KeyVal | MouseButton MouseButton
  deriving (Eq, Show)

data GLUTGtk = GLUTGtk
  { realizeCallback       :: IORef RealizeCallback
  , reshapeCallback       :: IORef ReshapeCallback
  , displayCallback       :: IORef DisplayCallback
  , keyboardMouseCallback :: IORef KeyboardMouseCallback
  , postRedisplay         :: IO ()
  , widget                :: EventBox
  }

glut :: EventBox -> Size -> IO GLUTGtk
glut eventb (Size width height) = do

  -- Initialize callback IORefs
  realizeCallback'       <- newIORef $ return ()
  displayCallback'       <- newIORef $ return ()
  reshapeCallback'       <- newIORef $ \_ -> return ()
  keyboardMouseCallback' <- newIORef $ \_ _ _ _ -> return ()

  -- Initialise canvas
  config <- glConfigNew [ GLModeRGBA, GLModeDouble ]
  canvas <- glDrawingAreaNew config
  widgetSetSizeRequest canvas width height

  -- Add canvas to Event box
  set eventb [ containerBorderWidth := 0, containerChild := canvas ]

  -- Update drawing when necessary
  _ <- onRealize canvas $ withGLDrawingArea canvas $ \_ -> do
         join (readIORef realizeCallback')

  -- update when size changes
  _ <- canvas `on` configureEvent $ tryEvent $ do
    (w, h) <- eventSize
    liftIO $ do
      cb <- readIORef reshapeCallback'
      cb (Size w h)

  -- update when it's exposed
  _ <- canvas `on` exposeEvent $ tryEvent $ liftIO $ withGLDrawingArea canvas $ \gl -> do
    join (readIORef displayCallback')
    glDrawableSwapBuffers gl

  -- Install key/mouse handlers
  let handleButton s = do
        b      <- eventButton
        (x, y) <- eventCoordinates
        ms     <- eventModifier
        liftIO $ do
          cb <- readIORef keyboardMouseCallback'
          cb (MouseButton b) s ms (Just (Position x y))
  let handleKey s = do
        v      <- eventKeyVal
        ms     <- eventModifier
        liftIO $ do
          cb <- readIORef keyboardMouseCallback'
          cb (Key v) s ms Nothing
  _ <- eventb `on` buttonPressEvent   $ tryEvent $ handleButton Down
  _ <- eventb `on` buttonReleaseEvent $ tryEvent $ handleButton Up
  _ <- eventb `on` keyPressEvent      $ tryEvent $ handleKey Down
  _ <- eventb `on` keyReleaseEvent    $ tryEvent $ handleKey Up

  -- 
  return $ GLUTGtk
    { realizeCallback       = realizeCallback'
    , displayCallback       = displayCallback'
    , reshapeCallback       = reshapeCallback'
    , keyboardMouseCallback = keyboardMouseCallback'
    , postRedisplay         = widgetQueueDraw canvas
    , widget                = eventb
    }
