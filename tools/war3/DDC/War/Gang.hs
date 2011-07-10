
module DDC.War.Gang
	( Gang
	, GangState(..)
	, getGangState
	, startGang
	, pauseGang
	, flushGang
	, waitForGangState
	, forkGangActions )
where
import Control.Concurrent
import Data.IORef

-- Gang -----------------------------------------------------------------------
data Gang
	= Gang 
	{ gangThreads		:: Int
	, gangThreadsAvailable	:: QSemN
	, gangState		:: IORef GangState
	, gangActionsRunning	:: IORef Int }

data GangState
	= -- | Gang is running actions.
	  GangRunning

	-- | Gang may be running already started actions, 
	--   but no new ones are being started
	| GangPaused

	-- | Gang is waiting for already started actions to be completed,
	--   but not new ones are being started.
	| GangFlushing

	-- | Gang is finished, all the actions have been run.
	| GangFinished
	deriving (Show, Eq)


-- | Get the state of a gang
getGangState :: Gang -> IO GangState
getGangState gang
 = readIORef (gangState gang)

-- | Start a paused gang, to allow it to continue running actions.
startGang :: Gang -> IO ()
startGang gang
 = writeIORef (gangState gang) GangRunning


-- | Pause a gang, to prevent it from running more actions.
pauseGang :: Gang -> IO ()
pauseGang gang
 = writeIORef (gangState gang) GangPaused


-- | Block until already started actions have completed, 
--   but don't start any more.
flushGang :: Gang -> IO ()
flushGang gang
 = do	writeIORef (gangState gang) GangFlushing
	waitForGangState gang GangFinished


-- | Sleep-wait until the gang has gone into the finished state.
waitForGangState :: Gang -> GangState -> IO ()
waitForGangState gang waitState
 = do	state	<- readIORef (gangState gang)
	if state == waitState
	 then return ()
	 else do
		threadDelay 100000
		waitForGangState gang waitState


-- | Start up a new gang to run the given actions.
forkGangActions
	:: Int 			-- ^ number of worker threads in the gang
	-> [IO ()] 		-- ^ actions to run, which 
	-> IO Gang

forkGangActions threads actions
 = do	semThreads		<- newQSemN threads
	refState		<- newIORef GangRunning
	refActionsRunning	<- newIORef 0
	let gang	
		= Gang
		{ gangThreads		= threads
		, gangThreadsAvailable	= semThreads 
		, gangState 		= refState
		, gangActionsRunning	= refActionsRunning }

	forkIO $ gangLoop gang actions
	return gang
	

-- | Run actions on a gang.
gangLoop :: Gang -> [IO ()] -> IO ()
gangLoop gang []
 = do	-- Wait for all the threads to finish.
	waitQSemN 
		(gangThreadsAvailable gang) 
		(gangThreads gang)
		
	-- Signal that the gang is finished running actions.
	writeIORef (gangState gang) GangFinished


gangLoop gang actions
 = do	state	<- readIORef (gangState gang)
	case state of
	 GangRunning 
	  -> do	-- Wait for a worker thread to become available.
		waitQSemN (gangThreadsAvailable gang) 1
		gangLoop_withWorker gang actions

	 GangPaused
	  -> do	threadDelay 100000
	 	gangLoop gang actions
			
	 GangFlushing
	  -> do	actionsRunning	<- readIORef (gangActionsRunning gang)
		if actionsRunning == 0
		 then	writeIORef (gangState gang) GangFinished
		 else do	
			threadDelay 100000
			gangLoop gang []

	 GangFinished
	  -> return ()


-- we have an available worker
gangLoop_withWorker gang actions@(action:actionsRest)
 = do	-- See if we're supposed to be starting actions or not.
	state	<- readIORef (gangState gang)
	case state of
	 GangRunning
	  -> do	forkOS $ do
			-- run the action (and wait for it to complete)
			result	<- action

			-- signal that a new worker is available
			signalQSemN (gangThreadsAvailable gang) 1
	
		gangLoop gang actionsRest

	 _ -> gangLoop gang actions

