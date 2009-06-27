
module Test.TestFail 
	( TestFail (..)
	, testFailName
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


	deriving (Eq, Show)


instance Error TestFail where
 strMsg s	= TestFailOther s


testFailName :: TestFail -> String
testFailName fail
 = case fail of
	TestIgnore{}			-> "ignore"
	TestFailOther{}			-> "other"
	TestFailIO{}			-> "io"
	TestFailBuild{}			-> "build"
	TestFailBuildSuccess{}		-> "build success"
	TestFailCompile{}		-> "compile"
	TestFailCompileSuccess{}	-> "compile success"
	TestFailRun{}			-> "run"
	TestFailMissingFile{}		-> "missing file"


pprTestFail :: TestFail -> String
pprTestFail fail
 = case fail of
	TestFailOther str		-> "other " ++ str
	TestIgnore{}			-> "ignore"
	TestFailIO iof			-> "io" ++ show iof
	TestFailBuild{}			-> "build"
	TestFailBuildSuccess{}		-> "fail"
	TestFailCompile{}		-> "compile"
	TestFailCompileSuccess{}	-> "fail"
	TestFailRun{}			-> "run"
	TestFailMissingFile path	-> "missing file " ++ show path


pprTestFailColor :: TestFail -> String
pprTestFailColor fail
	= setMode [Bold, Foreground Red] ++ pprTestFail fail ++ setMode [Reset]

