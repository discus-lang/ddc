
module Test.TestWin where

import Format
import Timing
import System.Time

import Util.Terminal.VT100

-------------------------------------------------------------------------------
data TestWin
	-- Generic Success
	= TestWinOk

	-- Build succeeded
	| TestWinBuild	
		{ testWinTime	:: TimeDiff
		, testWinSize	:: Int }

	-- Build failed, as expected
	| TestWinBuildError
		
	-- Compilation succeeded
	| TestWinCompile
		{ testWinTime	:: TimeDiff
		, testWinSize	:: Int }

	-- Compilation failed, as expected
	| TestWinCompileError

	-- Execution of shell script succeeded
	| TestWinShell
		{ testWinTime	:: TimeDiff }

	-- Execution of shell script failed, as expected
	| TestWinShellError

	-- Binary ran successfully
	| TestWinRun
		{ testWinTime	:: TimeDiff }

	-- Binary failed, as expected
	| TestWinRunError

	-- File was as expected.
	| TestWinDiff
	deriving (Show, Eq)


pprTestWinColor :: TestWin -> String
pprTestWinColor win 
 = case win of
	TestWinBuild{}
	 -> setMode [Bold, Foreground Blue] ++ pprTestWin win ++ setMode [Reset]

	TestWinCompile{}
	 -> setMode [Bold, Foreground Blue] ++ pprTestWin win ++ setMode [Reset]

	TestWinRun{}
	 -> setMode [Bold, Foreground Green] ++ pprTestWin win ++ setMode [Reset]

	_ -> pprTestWin win


pprTestWin :: TestWin -> String
pprTestWin win
 = case win of
	TestWinOk		-> "ok"

	TestWinBuild time size	
	  -> "time("  ++ pprTime time ++ "s)"
	
	TestWinBuildError	-> "ok"

	TestWinShell time
	  -> "time(" ++ pprTime time ++ "s)"

	TestWinShellError	-> "ok"

	TestWinCompile time size	
	  -> "time("  ++ pprTime time ++ "s)"

	TestWinCompileError	-> "ok"
	TestWinRun time		-> "time("   ++ pprTime time ++ "s)"
	TestWinDiff	  	-> "ok"


pprTime :: TimeDiff -> String
pprTime time = padL formatTimeWidth (pprTimeDiff time)

