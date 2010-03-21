{-# LANGUAGE NoMonomorphismRestriction #-}

module Util.Control.Dispatch 
	( DispatchAction
	, dispatchWork)
where
import Util.Control.Dispatch.Worker
import Control.Monad
import Control.Monad.State
import Control.Concurrent
import System.Exit
import System.IO
import Data.Set				(Set)
import Util.Data.WorkGraph		(WorkGraph)
import qualified Util.Data.WorkGraph	as WorkGraph
import qualified Data.Set		as Set


-- | What to do for each thread.
--	The MVars are, in order, used to receive tests to run and to
--	write results.
type DispatchAction job result
	=  MVar (job, [job])
	-> MVar (job, [job], result)
	-> IO ()

data DispatchState job result
	= DispatchState
		-- what to do when a job has finished
		{ stateHookFinished	:: job -> result -> IO ()

		-- check a result to see if the job failed
		, stateResultFailed	:: result -> Bool

		-- what to do when a job is ignored because a parent failed
		, stateHookIgnore	:: job -> IO ()
		
		-- the current workers
		, stateWorkers		:: Set (Worker job result)
		
		-- the current work graph
		, stateGraph		:: WorkGraph job
		
		-- the current preferences, we'd prefer to run these jobs,
		--	in order, before others.
		, statePrefs		:: [job]
		
		-- the jobs to ignore because their parents failed
		, stateIgnore		:: Set job
		
		-- the running jobs
		--	these remain in the graph, but we won't give them
		--	to a second worker.
		, stateRunning		:: Set job }



-- | Carries the state of the dispatcher
type Dispatcher job result a
	=  StateT (DispatchState job result) IO a


dispatchWork 
	:: (Show job, Ord job)
	=> (job -> result -> IO ())	-- hook on job finished
	-> (job -> IO ())		-- hook on job ignored
	-> (result -> Bool)		-- decide if a result failed or not
	-> WorkGraph job 
	-> Int
	-> DispatchAction job result
	-> IO ()

dispatchWork 
	hookFinish
	hookIgnore
	resultFailed
	graph
	threads
	action
 = do
	let makeWorker 
	     = do sVar	<- newEmptyMVar
		  rVar	<- newEmptyMVar
		  tid	<- forkIO $ action sVar rVar
		  return $ Worker tid False sVar rVar

	workers	<- liftM  Set.fromList
		$  replicateM threads makeWorker

	let state
		= DispatchState
		{ stateHookFinished	= hookFinish
		, stateHookIgnore	= hookIgnore
		, stateResultFailed	= resultFailed
		, stateWorkers		= workers
		, stateGraph		= graph
		, statePrefs		= []
		, stateIgnore		= Set.empty
		, stateRunning		= Set.empty }

	execStateT dispatchWork_run state
	return ()
	
		
dispatchWork_run :: (Show job, Ord job) => Dispatcher job result ()
dispatchWork_run 
 = do	graph		<- gets stateGraph
	tsRunning	<- gets stateRunning
	gotInput	<- liftIO $ hReady stdin

	let result
		-- We've finished all the tests, so our work here is done.
		| WorkGraph.null graph
		= return ()

		-- Abort the run if there is any user input.
		| gotInput
		= do	workers	<- gets stateWorkers

			liftIO	
			 $ do	putStr	"-- Interrupt. Waiting for running jobs (CTRL-C kills) ... "
				hFlush stdout

				mapM_ (killThread . workerThreadId) 
					$ Set.toList workers
	
				putStr	"done.\n"

			liftIO	exitSuccess

		| otherwise
		= dispatchWork_send

	result
	

dispatchWork_send :: (Show job, Ord job) => Dispatcher job result ()
dispatchWork_send 
  = do	workers		<- gets stateWorkers
	graph		<- gets stateGraph
	tsPref		<- gets statePrefs
	tsRunning	<- gets stateRunning
	tsIgnore	<- gets stateIgnore

	-- look for a free worker
	mFreeWorker	<- liftIO $ takeFirstFreeWorker $ Set.toList workers
	
	let result
		-- If there are no free workers then wait for one to finish.
		| Nothing	<- mFreeWorker
		= do	liftIO $ threadDelay 10000	-- 10ms
--			liftIO $ print graph
		
			dispatchWork_recv 

		-- Try to find a test to send.
		--	there is guaranteed to be a test in the graph
		| Just worker		<- mFreeWorker
		, mTestGraphChildren	<- WorkGraph.takeWorkPrefNot graph tsPref tsRunning
		= case mTestGraphChildren of

			Just (test, children, graph', tsPrefs')

			 -- There's an available test, but it's in the ignore set.
			 -- 	Skip over it and ignore all its children as well.
			 | Set.member test tsIgnore
			 -> do	
				-- run the ignore hook
				hookIgnore	<- gets stateHookIgnore 
				liftIO $ hookIgnore test
				
				-- also ignore the children of this ignored test
				modify 	$ \s -> s 
					{ stateGraph	= graph'
					, statePrefs	= tsPrefs'
					, stateIgnore	= Set.union (stateIgnore s) (Set.fromList children) }
						
				-- Check if any worker have finished in the mean-time				
				dispatchWork_recv 

			 -- There's an available test, and its not in the ignore set,
			 --	and we've got a free worker -- so sent it out.
			 | otherwise
			 -> do	
				liftIO $ putMVar (workerTestVar worker) (test, children)

				-- Mark the worker as currently busy
				let workers'
					= Set.insert (worker { workerIsBusy = True })
					$ Set.delete worker workers

				-- Mark the sent test as currently running
				modify 	$ \s -> s 
					{ stateWorkers	= workers'
--					, stateGraph	= graph'
					, statePrefs	= tsPrefs'
					, stateRunning	= Set.insert test tsRunning }

				-- Check if any workers have finished in the mean-time
				dispatchWork_recv 

			-- We have a free worker, and there are tests in the graph
			--	but none of them are available to be sent at the moment.
			--	We'll need to wait for some workers to finish what they're currently doing
			Nothing
			 ->	dispatchWork_recv 
	result

dispatchWork_recv :: (Show job, Ord job) => Dispatcher job result ()
dispatchWork_recv
 = do	workers		<- gets stateWorkers
	graph		<- gets stateGraph

	mWorkerResult	<- liftIO $ takeFirstWorkerResult $ Set.toList workers

	let cont
		-- no workers have a result, wait for a bit then loop again
		| Nothing	<- mWorkerResult
		= dispatchWork_run 

		-- a worker returned a result
		| Just (worker, test, tsChildren, result)	<- mWorkerResult
		= do	
			-- run the finishing hook
			hookFinished	<- gets stateHookFinished
			liftIO $ hookFinished test result
			
			-- mark the worker as free again
			modify 	$ \s -> s 
				{ stateWorkers	= Set.insert (setWorkerAsFree worker) workers	
				, stateRunning	= Set.delete test (stateRunning s) 
				, stateGraph	= WorkGraph.deleteRootNode test graph }

			resultFailed	<- gets stateResultFailed
			if resultFailed result
			 then do
				-- ignore all the children of this test
				modify 	$ \s -> s
					{ stateIgnore = Set.union (stateIgnore s) (Set.fromList tsChildren) }
					
				dispatchWork_run

			 else do
				-- prefer to run the tests children
				modify	$ \s -> s
					{ statePrefs	= statePrefs s ++ tsChildren }

			  	dispatchWork_run 
	cont
