
module DDC.War.Gang
	(runParActions)
where
import Control.Concurrent

data Gang
	= Gang 
	{ _gangThreads	:: Int
	, _gangSem	:: QSemN }


runParActions :: Int -> [Int -> IO ()] -> IO ()
runParActions threads actions
 = do	sem	<- newQSemN threads
	runActionsOnGang 0 (Gang threads sem) actions

runActionsOnGang :: Int -> Gang -> [Int -> IO ()] -> IO ()
runActionsOnGang _ (Gang threads sem) []
 = do	-- Wait for all the threads to finish.
	waitQSemN sem threads

runActionsOnGang ix gang@(Gang _ sem) (action:actionsRest)
 = do	
	-- wait for a thread to become available
	waitQSemN sem 1

	forkIO $ do
		-- run the action
		result	<- action ix
		-- signal that a new worker is available
		signalQSemN sem 1
	
	runActionsOnGang (ix + 1) gang actionsRest

