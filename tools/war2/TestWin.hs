
module TestWin where

import Format
import Timing
import System.Time

import Util.Terminal.VT100

-------------------------------------------------------------------------------
data TestWin
	= TestWinBuildMain	
		{ testWinTime	:: ClockTime
		, testWinSize	:: Int }

	| TestWinRun
		{ testWinTime	:: ClockTime }

	| TestWinDiffOk
	deriving (Show, Eq)

pprTestWinColor :: TestWin -> String
pprTestWinColor win 
 = case win of
	TestWinBuildMain{}
	 -> setMode [Bold, Foreground Cyan] ++ pprTestWin win ++ setMode [Reset]
	
	TestWinRun{}
	 -> setMode [Foreground Cyan]	    ++ pprTestWin win ++ setMode [Reset]

	_ -> pprTestWin win

pprTestWin :: TestWin -> String
pprTestWin win
 = case win of
	TestWinBuildMain time size	
	  -> "time("  ++ pprTime time ++ "s)"  ++ " size(" ++ show size ++ ")"

	TestWinRun	 time		
	  -> "time("   ++ pprTime time ++ "s)"

	TestWinDiffOk
	  -> "ok"

pprTime :: ClockTime -> String
pprTime time = padL formatTimeWidth (pprClockTime time)