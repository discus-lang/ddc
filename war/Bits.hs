{-# OPTIONS -fno-warn-missing-methods #-}

-- | odds and ends

module Bits 
	(Arg(..)
	, timeIO
	, timeIO_)
where

import System.Time
import Util

-- Arguments to DDC --------------------------------------------------------------------------------
data Arg
	= ArgFlagsDDC [String]
	| ArgFlagsGHC [String]
	| ArgDebug


-- Timing ------------------------------------------------------------------------------------------
instance Pretty ClockTime where
 ppr (TOD sec_ psec_)
  = let	psecs	= sec_ * 10^12 + psec_
    in	(psecs `div` 10^12) % "." % (take 3 $ padLc '0' 12 $ show $ psecs `mod` 10^12)

instance Num ClockTime where
 (TOD s1 p1) - (TOD s2 p2)	= TOD (s1 - s2) (p1 - p2)
 (TOD s1 p1) + (TOD s2 p2)	= TOD (s1 + s2) (p1 + p2)	 
	

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
