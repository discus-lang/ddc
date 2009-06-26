
module Dispatch 
	( DispatchAction
	, DispatchConfig(..)
	, dispatchWork_run)
	
where

import Dispatch.Worker
import qualified Dispatch.WorkGraph	as WorkGraph
import Dispatch.WorkGraph		(WorkGraph, WorkNode(..))
import qualified Data.Map		as Map
import Data.Map				(Map)
import qualified Data.Set		as Set
import Data.Set				(Set)

import Control.Concurrent
import Control.Concurrent.MVar
import System.Exit
import System.IO

-- | What to do for each thread.
type DispatchAction job result
	=  MVar (job, [job]) 			-- ^ MVar to receive tests to run
	-> MVar (job, [job], result) 		-- ^ MVar to write results to
	-> IO ()


data DispatchConfig job result
	= DispatchConfig 
		{ dispatchHookIgnore	:: job -> IO ()
		, dispatchHookFinished	:: job -> result -> IO () 
		, dispatchResultFailed	:: result -> Bool }


-- | Carries the state of the dispatcher
type Dispatcher job result
	=  DispatchConfig job result
	-> Set (Worker job result)		-- ^ current workers
	-> WorkGraph job			-- ^ work graph
	-> Set job				-- ^ jobs to ignore
	-> [job]				-- ^ preferences
	-> Set job				-- ^ running jobs
	-> IO ()

dispatchWork_run :: Ord job => Dispatcher job result
dispatchWork_run config workers graph tsIgnore tsPref tsRunning
	
	-- We've finished all the tests, 
	--	so our work here is done.
	| WorkGraph.null graph
	= return ()

	-- Abort the run if there is any user input.
	| otherwise
	= do	ready	<- hReady stdin
		if ready
	 	 then	exitSuccess
	 	 else	dispatchWork_send config workers graph tsIgnore tsPref tsRunning

dispatchWork_send :: Ord job => Dispatcher job result
dispatchWork_send config workers graph tsIgnore tsPref tsRunning
  = do	
	-- look for a free worker
	mFreeWorker	<- takeFirstFreeWorker 
			$ Set.toList workers
	
	let result
		-- If there are no free workers then wait for one to finish.
		| Nothing	<- mFreeWorker
		= do	threadDelay 10000	-- 10ms
			dispatchWork_recv config workers graph tsIgnore tsPref tsRunning

		-- Try to find a test to send.
		--	there is guaranteed to be a test in the graph
		| Just worker		<- mFreeWorker
		, mTestGraphChildren	<- WorkGraph.takeWorkPrefNot 
						graph 
						tsPref 
						tsRunning
		= case mTestGraphChildren of

			Just (test, children, graph', tsPref')

			 -- There's an available test, but it's in the ignore set.
			 -- 	Skip over it and ignore all its children as well.
			 | Set.member test tsIgnore
			 -> do	
				-- run the ignore hook
				dispatchHookIgnore config test

				-- Check if any worker have finished in the mean-time
				let tsIgnore'	= Set.union tsIgnore (Set.fromList children)
				dispatchWork_recv config workers graph' tsIgnore' tsPref' tsRunning

			 -- There's an available test, and its not in the ignore set,
			 --	and we've got a free worker -- so sent it out.
			 | otherwise
			 -> do	
				putMVar (workerTestVar worker) (test, children)

				-- Mark the worker as currently busy
				let workers'
					= Set.insert (worker { workerIsBusy = True })
					$ Set.delete worker 
					$ workers

				-- Mark the sent test as currently running
				let tsRunning'	= Set.insert test tsRunning

				-- check if any workers have finished in the mean-time
				dispatchWork_recv config workers' graph' tsIgnore tsPref' tsRunning'


			-- We have a free worker, and there are tests in the graph
			--	but none of them are available to be sent at the moment.
			--	We'll need to wait for some workers to finish what they're currently doing
			Nothing
			 ->	dispatchWork_recv config workers graph tsIgnore tsPref tsRunning

	result

dispatchWork_recv :: Ord job => Dispatcher job result
dispatchWork_recv config workers graph tsIgnore tsPrefs tsRunning
 = do	mWorkerResult	<- takeFirstWorkerResult 
			$  Set.toList workers

	let cont
		-- no workers have a result, wait for a bit then loop again
		| Nothing	<- mWorkerResult
		= do	dispatchWork_run config workers graph tsIgnore tsPrefs tsRunning

		-- a worker returned a result
		| Just (worker, test, tsChildren, result)	<- mWorkerResult
		= do
			dispatchHookFinished config test result

			-- mark the worker as free again
			let workers'	= Set.insert (setWorkerAsFree worker) workers	
			let tsRunning'	= Set.delete test tsRunning

			if (dispatchResultFailed config result)
			 then do
				-- ignore all the children of this test
				let tsIgnore'	= Set.union tsIgnore (Set.fromList tsChildren)
 	 			dispatchWork_run config workers' graph tsIgnore' tsPrefs tsRunning'

			 else do
				-- prefer to run the tests children
				let tsPrefs'	= tsPrefs ++ tsChildren
			  	dispatchWork_run config workers' graph tsIgnore tsPrefs' tsRunning'

	cont
