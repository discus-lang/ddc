-- | Constructor Tags.
module Churn.Tag 
where

import GHC.Base

-- | Holds a constructor tag for a certain type.
data Tag a
	= Tag Int#

instance Show (Tag a) where
	show (Tag i)	= "#" ++ show (I# i)
	
-- | Take the outermost constructor tag from this object.
tag :: a -> Tag a
tag x	= Tag (GHC.Base.getTag x)