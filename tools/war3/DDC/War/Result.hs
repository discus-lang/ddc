{-# LANGUAGE GADTs #-}

module DDC.War.Result
	( Result(..)
	, isResultUnexpectedFailure
	, isResultUnexpectedSuccess
	, takeResultTime
	, takeResultDiff)
	
	
where
import BuildBox
import Data.Maybe

data Result
	= ResultUnexpectedFailure
	| ResultUnexpectedSuccess
	| ResultAspect 	(WithUnits (Aspect Single))
	| ResultQuirk  	Quirk
	| ResultDiff   	String String
	deriving Show

isResultUnexpectedFailure :: Result -> Bool
isResultUnexpectedFailure rr
 = case rr of
	ResultUnexpectedFailure{}	-> True
	_				-> False

isResultUnexpectedSuccess :: Result -> Bool
isResultUnexpectedSuccess rr
 = case rr of
	ResultUnexpectedSuccess{}	-> True
	_				-> False

takeResultTime :: [Result] -> Maybe Seconds
takeResultTime as
 	= listToMaybe [t | ResultAspect (WithSeconds (Time TotalWall (Single t))) <- as]


takeResultDiff :: [Result] -> Maybe (String, String)
takeResultDiff as
 	= listToMaybe [(fileOut, fileDiff) | a@(ResultDiff fileOut fileDiff) <- as]
