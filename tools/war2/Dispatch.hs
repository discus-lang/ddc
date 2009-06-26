
module Dispatch 
	(Worker (..))
where

import Test

import Control.Concurrent
import Control.Concurrent.MVar

data Worker
	= Worker 
		{ workerThreadId 	:: ThreadId
		, workerIsBusy		:: Bool
		, workerTestVar		:: MVar Test
		, workerResultVar	:: MVar (Test, Either TestFail TestWin) }
	

instance Eq Worker where
	w1 == w2	= workerThreadId w1 == workerThreadId w2

instance Ord Worker where
	compare w1 w2	= compare (workerThreadId w1) (workerThreadId w2)
