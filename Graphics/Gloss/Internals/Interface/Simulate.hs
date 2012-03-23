{-# LANGUAGE RankNTypes #-}

module Graphics.Gloss.Internals.Interface.Simulate
	(simulateWithBackendIO)
where
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Internals.Render.Picture
import Graphics.Gloss.Internals.Render.ViewPort
import Graphics.Gloss.Internals.Interface.Backend
import Graphics.Gloss.Internals.Interface.Window
import Graphics.Gloss.Internals.Interface.Common.Exit
import Graphics.Gloss.Internals.Interface.ViewPort
import Graphics.Gloss.Internals.Interface.ViewPort.KeyMouse
import Graphics.Gloss.Internals.Interface.ViewPort.Motion
import Graphics.Gloss.Internals.Interface.ViewPort.Reshape
import Graphics.Gloss.Internals.Interface.Animate.Timing
import Graphics.Gloss.Internals.Interface.Simulate.Idle
import qualified Graphics.Gloss.Internals.Interface.Callback			as Callback
import qualified Graphics.Gloss.Internals.Interface.ViewPort.ControlState	as VPC
import qualified Graphics.Gloss.Internals.Interface.Simulate.State		as SM
import qualified Graphics.Gloss.Internals.Interface.Animate.State		as AN
import qualified Graphics.Gloss.Internals.Render.State	        		as RS
import Data.IORef
import System.Mem


simulateWithBackendIO
	:: forall model a
	.  Backend a
	=> a				-- ^ Initial state of the backend
        -> Display                      -- ^ Display mode.
	-> Color			-- ^ Background color.
	-> Int				-- ^ Number of simulation steps to take for each second of real time.
	-> model 			-- ^ The initial model.
	-> (model -> IO Picture)	 	-- ^ A function to convert the model to a picture.
	-> (ViewPort -> Float -> model -> IO model) -- ^ A function to step the model one iteration. It is passed the
						 --	current viewport and the amount of time for this simulation
						 --     step (in seconds).
	-> IO ()

simulateWithBackendIO
	backend
        display
	backgroundColor
	simResolution
	worldStart
	worldToPicture
	worldAdvance
 = do
	let singleStepTime	= 1

	-- make the simulation state
	stateSR		<- newIORef $ SM.stateInit simResolution

	-- make a reference to the initial world
	worldSR		<- newIORef worldStart

	-- make the initial GL view and render states
	viewSR		<- newIORef viewPortInit
	viewControlSR	<- newIORef VPC.stateInit
	animateSR	<- newIORef AN.stateInit
        renderS_        <- RS.stateInit
	renderSR	<- newIORef renderS_

	let displayFun backendRef
	     = do
		-- convert the world to a picture
		world		<- readIORef worldSR
		picture	        <- worldToPicture world
	
		-- display the picture in the current view
		renderS		<- readIORef renderSR
		viewS		<- readIORef viewSR

		-- render the frame
		withViewPort 
			backendRef
			viewS
	 	 	(renderPicture backendRef renderS viewS picture)
 
		-- perform garbage collection
		performGC

	let callbacks
	     = 	[ Callback.Display	(animateBegin animateSR)
		, Callback.Display 	displayFun
		, Callback.Display	(animateEnd   animateSR)
		, Callback.Idle		(callback_simulate_idle 
						stateSR animateSR viewSR 
						worldSR worldStart worldAdvance
						singleStepTime)
		, callback_exit () 
		, callback_viewPort_keyMouse viewSR viewControlSR 
		, callback_viewPort_motion   viewSR viewControlSR 
		, callback_viewPort_reshape ]

	createWindow backend display backgroundColor callbacks
