
module Test.TestFail 
	( TestFail (..)
	, pprTestFail
	, pprTestFailColor)
where

import Command
import Util.Terminal.VT100
import Control.Monad.Error

data TestFail
	-- Test failed for some mysterious reason
	= TestFailOther		String

	-- Test was ignored becuase one of its parents failed
	| TestIgnore

	-- Framework problems -------------------------------------------------
	-- A miscelaneous IO command failed.
	| TestFailIO		IOFail

	-- Test failed because a file was missing
	| TestFailMissingFile 	FilePath

	-- Common failures ----------------------------------------------------
	-- Build failed
	| TestFailBuild
		{ testFailIOFail	:: IOFail	-- the io action for the compilation
		, testFailOutFile	:: FilePath	-- file of stdout log
		, testFailErrFile	:: FilePath }	-- file of stderr log

	-- We were expecting the build to fail, but it didn't.
	| TestFailBuildSuccess
		{ testFailOutFile	:: FilePath
		, testFailErrFile	:: FilePath}

	-- Compilation of a source file failed
	| TestFailCompile
		{ testFailIOFail	:: IOFail	-- the io action for the compilation
		, testFailOutFile	:: FilePath	-- file of stdout log
		, testFailErrFile	:: FilePath }	-- file of stderr log

	-- We were expecting the compile to fail, but it didn't.
	| TestFailCompileSuccess
		{ testFailOutFile	:: FilePath
		, testFailErrFile	:: FilePath}

	-- Running a binary failed
	| TestFailRun
		{ testFailIOFail	:: IOFail	-- the io action for the compilation
		, testFailOutFile	:: FilePath	-- file of stdout log
		, testFailErrFile	:: FilePath }	-- file of stderr log

	-- Output file was different to expected
	| TestFailDiff
		{ testFailExpectedFile	:: FilePath	-- expected output
		, testFailActualFile	:: FilePath	-- actual output
		, testFailDiffFile	:: FilePath }	-- file containing differences

	deriving (Eq, Show)


instance Error TestFail where
 strMsg s	= TestFailOther s


pprTestFail :: TestFail -> String
pprTestFail fail
 = case fail of
	TestFailOther str		-> "other " ++ str
	TestIgnore{}			-> "ignored"
	TestFailIO iof			-> "framework io error " ++ show iof
	TestFailMissingFile path	-> "framework missing file " ++ show path
	TestFailBuild{}			-> "build failed"
	TestFailBuildSuccess{}		-> "unexpected build success"
	TestFailCompile{}		-> "compile failed"
	TestFailCompileSuccess{}	-> "unexpected compile success"
	TestFailRun{}			-> "run failed"
	TestFailDiff{}			-> "diff"


pprTestFailColor :: TestFail -> String
pprTestFailColor fail
	= setMode [Bold, Foreground Red] ++ pprTestFail fail ++ setMode [Reset]

