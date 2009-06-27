
module Test.TestWin where

import Format
import Timing
import System.Time

import Util.Terminal.VT100

-------------------------------------------------------------------------------
data TestWin

	-- Build succeeded
	= TestWinBuild	
		{ testWinTime	:: ClockTime
		, testWinSize	:: Int }

	-- Build failed, as expected
	| TestWinBuildError
		
	-- Compilation succeeded
	| TestWinCompile
		{ testWinTime	:: ClockTime
		, testWinSize	:: Int }

	-- Compilation failed, as expected
	| TestWinCompileError

	-- Binary ran successfully
	| TestWinRun
		{ testWinTime	:: ClockTime }

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
	TestWinBuild time size	
	  -> "time("  ++ pprTime time ++ "s)"  ++ " size(" ++ show size ++ ")"
	
	TestWinBuildError	-> "ok"

	TestWinCompile time size	
	  -> "time("  ++ pprTime time ++ "s)"  ++ " size(" ++ show size ++ ")"

	TestWinCompileError	-> "ok"
	TestWinRun time		-> "time("   ++ pprTime time ++ "s)"
	TestWinDiff	  	-> "ok"


pprTime :: ClockTime -> String
pprTime time = padL formatTimeWidth (pprClockTime time)

