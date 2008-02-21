{-# OPTIONS -fno-warn-missing-methods #-}

-- | odds and ends

module Bits 
	(Arg(..)
	, timeIO
	, timeIO_
	, parseArgs
	, psecsOfClockTime)
where

import Shared.Pretty
import System.Time

import Util.Options	as Options
import Util

-- Arguments to DDC --------------------------------------------------------------------------------
data Arg
	= ArgDebug			-- print debugging messages about the test driver
	| ArgFlagsDDC  [String]		-- flags to pass to DDC when compiling test files
	| ArgTestDirs  [String]		-- only run on these test dirs
	| ArgTestWith  [String]		-- a set of DDC flags to test with
	| ArgHelp
	deriving (Show, Eq)

instance Pretty Arg a where
 ppr	= ppr . show

options
 = 	[ ODefault	ArgTestDirs
 	, OFlag		ArgHelp
 			[ "-h", "-help", "--help"]
			"Display this help"

	, OOpts		(\ss -> ArgFlagsDDC $ map ('-' :) ss)
			[ "-ddc"]
			"-ddc <options>"
			"Turn on these DDC compile options."
			
	, OOpts		(\ss -> ArgTestWith $ map ('-' :) ss)
			[ "-with" ]
			"-with <options>"
			"Test with these DDC compile options." ]
			
parseArgs :: [String] -> ([String], [Arg])
parseArgs args	= Options.munch options $ Options.tokenise $ catInt " " args


-- Timing ------------------------------------------------------------------------------------------
instance Pretty ClockTime PMode where
 ppr (TOD sec_ psec_)
  = let	psecs	= sec_ * 10^12 + psec_
    in	(psecs `div` 10^12) % "." % (take 3 $ (pprStrPlain $ padRc 12 '0' $ show $ psecs `mod` 10^12))

instance Num ClockTime where
 (TOD s1 p1) - (TOD s2 p2)	= TOD (s1 - s2) (p1 - p2)
 (TOD s1 p1) + (TOD s2 p2)	= TOD (s1 + s2) (p1 + p2)	 
	
-- find the absolute number of pico secs in a clocktime
psecsOfClockTime :: ClockTime -> Integer
psecsOfClockTime (TOD sec psec)
	= sec * 10^12 + psec


-- | Time an action, returning the result along with the time it took to run.
timeIO :: IO a -> IO (a, ClockTime)
timeIO action
 = do	timeStart	<- getClockTime
	x		<- action 
	timeEnd		<- getClockTime

	return (x, timeEnd - timeStart)

-- | Time an action, and only return the time it took to run
timeIO_ :: IO a -> IO ClockTime
timeIO_ action
 =	liftM snd $ timeIO action
