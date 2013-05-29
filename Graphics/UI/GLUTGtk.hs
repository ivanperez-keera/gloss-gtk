{-# LANGUAGE ExistentialQuantification #-}
-- | FIXME: iperez: This code was taken from pastebin. Although it is publicly
-- available, its redistribution can present a violation of the copyright law.
-- It should be substituted by our own code or simply removed.
module Graphics.UI.GLUTGtk
   ( module Graphics.UI.GLUTGtk
   , KeyVal, MouseButton(..), Modifier(..), ScrollDirection(..), Click(..)
   )
  where

import  Control.Monad           (join, when)
import  Control.Monad.Trans     (liftIO)
import  Data.IORef              (IORef,newIORef, readIORef)
import  Graphics.UI.Gtk         hiding (Size, get)
import  Graphics.UI.Gtk.OpenGL

type RealizeCallback = IO ()
type ReshapeCallback = Size -> IO ()
type DisplayCallback = IO ()
type KeyboardMouseCallback = Key -> KeyState -> [Modifier] -> (Maybe Position) -> IO ()
type MouseMoveCallback     = Position -> IO ()

data Size = Size Int Int
  deriving (Eq, Ord, Show)

data Position = Position Double Double
  deriving (Eq, Ord, Show)

data KeyState = Down | Up
  deriving (Eq, Ord, Show)

data Key = Key String | MouseButton MouseButton Click | MouseScroll ScrollDirection
  deriving (Eq, Show)

data GLUTGtk = forall a . ContainerClass a => GLUTGtk
  { realizeCallback       :: IORef RealizeCallback
  , reshapeCallback       :: IORef ReshapeCallback
  , displayCallback       :: IORef DisplayCallback
  , keyboardMouseCallback :: IORef KeyboardMouseCallback
  , mouseMoveCallback     :: IORef MouseMoveCallback
  , postRedisplay         :: IO ()
  , widget                :: a
  , glCanvas              :: GLDrawingArea
  }

glut :: ContainerClass a => a -> Size -> IO GLUTGtk
glut container (Size width height) = do

  -- Initialize callback IORefs
  realizeCallback'       <- newIORef $ return ()
  displayCallback'       <- newIORef $ return ()
  reshapeCallback'       <- newIORef $ \_ -> return ()
  keyboardMouseCallback' <- newIORef $ \_ _ _ _ -> return ()
  mouseMoveCallback'     <- newIORef $ \_ -> return ()

  -- Initialise canvas
  config <- glConfigNew [ GLModeRGBA, GLModeDouble ]
  canvas <- glDrawingAreaNew config
  widgetSetSizeRequest canvas width height
  set canvas [ widgetCanFocus := True ]
  set container [ widgetCanFocus := True ]

  -- Add canvas to Event box
  set container [ containerBorderWidth := 0]
  containerAdd container canvas

  widgetAddEvents canvas [ PointerMotionMask, PointerMotionHintMask
                         , ButtonPressMask,   ButtonMotionMask
			 , Button1MotionMask, Button2MotionMask
                         , Button3MotionMask, KeyPressMask, ScrollMask
			 , KeyReleaseMask
			 ]

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
        clk    <- eventClick
        (x, y) <- eventCoordinates
        ms     <- eventModifier
        when (clk `elem` [SingleClick, DoubleClick, ReleaseClick]) $ 
          liftIO $ do
            cb <- readIORef keyboardMouseCallback'
            cb (MouseButton b clk) s ms (Just (Position x y))
  let handleKey s = do
        v      <- eventKeyName
        ms     <- eventModifier
        liftIO $ do
          cb <- readIORef keyboardMouseCallback'
          cb (Key v) s ms Nothing

  let handleScroll = do
        d      <- eventScrollDirection
        (x, y) <- eventCoordinates
        ms     <- eventModifier
        liftIO $ do
          cb <- readIORef keyboardMouseCallback'
          cb (MouseScroll d) Down ms (Just (Position x y))

  _ <- canvas `on` buttonPressEvent   $ tryEvent $ handleButton Down
  _ <- canvas `on` buttonReleaseEvent $ tryEvent $ handleButton Up
  _ <- canvas `on` keyPressEvent      $ tryEvent $ handleKey Down
  _ <- canvas `on` keyReleaseEvent    $ tryEvent $ handleKey Up
  _ <- canvas `on` scrollEvent        $ tryEvent $ handleScroll

  let handleMousemove = do
        (x,y) <- eventCoordinates
        liftIO $ do
           cb <- readIORef mouseMoveCallback'
           cb (Position x y)

  -- _ <- eventb `on` motionNotifyEvent  $ tryEvent $ handleMousemove
  _ <- canvas `on` motionNotifyEvent  $ tryEvent $ handleMousemove

  -- 
  return $ GLUTGtk
    { realizeCallback       = realizeCallback'
    , displayCallback       = displayCallback'
    , reshapeCallback       = reshapeCallback'
    , keyboardMouseCallback = keyboardMouseCallback'
    , mouseMoveCallback     = mouseMoveCallback'
    , postRedisplay         = widgetQueueDraw canvas
    , widget                = container
    , glCanvas              = canvas
    }
