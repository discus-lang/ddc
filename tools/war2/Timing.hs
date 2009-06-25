{-# OPTIONS -fno-warn-missing-methods #-}

module Timing 
	( psecsOfClockTime
	, pprClockTime
	, timeIO
	, timeIO_)
where


import System.Time
import Control.Monad

-- Timing ------------------------------------------------------------------------------------------
pprClockTime (TOD sec_ psec_)
  = let	psecs	= sec_ * 10^12 + psec_
    in	show (psecs `div` 10^12) 
		++ "." 
		++ (take 3 $ padRc 12 '0' $ show $ psecs `mod` 10^12)

padRc :: Int -> Char -> String -> String
padRc n c str
	= str ++ replicate (n - length str) c

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
