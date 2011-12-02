{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Unique identifiers for a type equivalence class.
module DDC.Type.Exp.ClassId
	(ClassId(..))
where
import Data.Ix
import Data.Hashable


-- | A unique identifier for a type equivalence class.
newtype ClassId	
	= ClassId Int
	deriving (Show, Eq, Ord)


instance Ix ClassId where
 range	   (ClassId a, ClassId b) 		= map ClassId [a..b]
 index	   (ClassId _, ClassId _) (ClassId x)	= x
 inRange   (ClassId a, ClassId b) (ClassId x)	= inRange (a, b) x
 rangeSize (ClassId a, ClassId b)		= rangeSize (a, b)
 
instance Hashable ClassId where
 hash 	(ClassId cid)				= hashInt cid
