
module TestFail 
	( TestFail (..)
	, testFailName
	, pprTestFail)
where

import Command
import Control.Monad.Error

data TestFail
	
	-- Test failed for some mysterious reason
	= TestFailOther		String

	-- Test was ignored becuase one of its parents failed
	| TestIgnore

	-- A miscelaneous IO command failed.
	--	This will be a problem with the test framework.
	| TestFailIO		IOFail

	-- Compilation of a source file failed
	| TestFailCompile
		{ testFailIOFail	:: IOFail	-- the io action for the compilation
		, testFailOutFile	:: FilePath	-- file of stdout log
		, testFailErrFile	:: FilePath }	-- file of stderr log

	-- Running a binary failed
	| TestFailRun
		{ testFailIOFail	:: IOFail	-- the io action for the compilation
		, testFailOutFile	:: FilePath	-- file of stdout log
		, testFailErrFile	:: FilePath }	-- file of stderr log

	-- Test failed because a file was missing
	| TestFailMissingFile FilePath

	deriving (Eq, Show)


instance Error TestFail where
 strMsg s	= TestFailOther s


testFailName :: TestFail -> String
testFailName fail
 = case fail of
	TestIgnore{}			-> "ignore"
	TestFailOther{}			-> "other"
	TestFailIO{}			-> "io"
	TestFailCompile{}		-> "compile"
	TestFailRun{}			-> "run"
	TestFailMissingFile{}		-> "missing file"


pprTestFail :: TestFail -> String
pprTestFail fail
 = case fail of
	TestIgnore{}			-> "ignore"
	TestFailOther str		-> "other " ++ str
	TestFailCompile iof out err	-> "compile"
	TestFailRun     iof out err	-> "run"
	TestFailMissingFile path	-> "missing file " ++ show path

