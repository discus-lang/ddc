
module DDC.War.Aspect
	(Aspect(..))
where
import Data.Time

data Aspect
	= AspectTime NominalDiffTime
	deriving (Show)
	
