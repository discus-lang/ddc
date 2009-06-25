
module Test where

import Config
import Command
import Timing

import System.Time
import Control.Monad.Error

data Test
	= TestBuildMain FilePath
	| TestRunBinary FilePath
	deriving (Eq, Ord, Show)

data TestResult
	= TestSuccess
	| TestFailed TestFail
	deriving (Eq, Show)

data TestFail
	= TestFailOther		String
	| TestFailIO		IOFail
	| TestFailInfo		Test [TestInfo]
	deriving (Eq, Show)

data TestInfo
	= TestInfoCommand	String			-- the command that was run that caused this failure
	| TestInfoIOFail	IOFail			-- the io action that failed
	| TestInfoOutFile	FilePath		-- file of stdout log
	| TestInfoErrFile	FilePath		-- file of stderr log
	deriving (Eq, Show)

instance Error TestFail where
 strMsg s	= TestFailOther s

