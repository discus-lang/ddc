
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
	TestFailOther str		-> "other " ++ str
	TestIgnore{}			-> "ignore"
	TestFailIO      iof		-> "io" ++ show iof
	TestFailCompile iof out err	-> "compile"
	TestFailRun     iof out err	-> "run"
	TestFailMissingFile path	-> "missing file " ++ show path


pprTestFailColor :: TestFail -> String
pprTestFailColor fail
	= setMode [Bold, Foreground Red] ++ pprTestFail fail ++ setMode [Reset]

