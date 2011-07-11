{-# OPTIONS -fno-warn-missing-methods #-}

module Timing 
	( pprTimeDiff
	, timeDiffToPicoSec
	, timeIO
	, timeIO_)
where


import System.Time
import Control.Monad

-- Timing ------------------------------------------------------------------------------------------
pprTimeDiff :: TimeDiff -> String
pprTimeDiff timeDiff
  = let	psecs	= timeDiffToPicoSec timeDiff
    in	show (psecs `div` 10^(12::Integer)) 
		++ "." 
		++ (take 3 $ padRc 12 '0' $ show $ psecs `mod` 10^(12::Integer))


timeDiffToPicoSec :: TimeDiff -> Integer
timeDiffToPicoSec td
	| 0	<- tdYear td
	, 0	<- tdMonth td
	= fromIntegral (tdDay  td)	* 10^(12::Integer) * 60 * 60 * 24
	+ fromIntegral (tdHour td)	* 10^(12::Integer) * 60 * 60
	+ fromIntegral (tdMin  td)	* 10^(12::Integer) * 60
	+ fromIntegral (tdSec  td) 	* 10^(12::Integer)
	+ tdPicosec td

 
padRc :: Int -> Char -> String -> String
padRc n c str
	= replicate (n - length str) c ++ str
	

-- | Time an action, returning the result along with the time it took to run.
timeIO :: IO a -> IO (a, TimeDiff)
timeIO action
 = do	timeStart	<- getClockTime
	x		<- action 
	timeEnd		<- getClockTime

	let timeDiff	= diffClockTimes timeEnd timeStart

	return (x, timeDiff)

-- | Time an action, and only return the time it took to run
timeIO_ :: IO a -> IO TimeDiff
timeIO_ action
 =	liftM snd $ timeIO action
