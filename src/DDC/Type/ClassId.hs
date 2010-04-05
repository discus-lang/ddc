
-- | A unique identifier for a type equivalence class.
module DDC.Type.ClassId
	(ClassId(..))
where
import Data.Ix


-- | A unique identifier for a type equivalence class.
newtype ClassId	
	= ClassId Int
	deriving (Show, Eq, Ord)


instance Ix ClassId where
 range	   (ClassId a, ClassId b) 		= map ClassId [a..b]
 index	   (ClassId a, ClassId b) (ClassId x)	= x
 inRange   (ClassId a, ClassId b) (ClassId x)	= inRange (a, b) x
 rangeSize (ClassId a, ClassId b)		= rangeSize (a, b)
 

