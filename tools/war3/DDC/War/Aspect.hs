
module DDC.War.Aspect
	( Aspect(..)
	, takeAspectTime 
	, takeAspectDiff)
where
import Data.Time
import Data.Maybe

data Aspect

	-- | A timed job took this amount of time.
	= AspectTime NominalDiffTime

	-- | The output of a job was different than what we expected
	--   from the baseline file.
	| AspectDiff 
		{ aspectFileBaseLine	:: FilePath
		, aspectFileActual	:: FilePath
		, aspectFileDiff	:: FilePath }
		
	deriving (Show)
	
takeAspectTime :: [Aspect] -> Maybe Aspect
takeAspectTime as
 	= listToMaybe [a | a@AspectTime{} <- as]

takeAspectDiff :: [Aspect] -> Maybe Aspect
takeAspectDiff as
 	= listToMaybe [a | a@AspectDiff{} <- as]