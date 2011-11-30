
module DDCI.Core.Prim.Name where
import DDC.Base.Pretty
        
data Name = Name String

instance Pretty Name where
 ppr (Name str) = text str

instance Ord Name where
 compare (Name s1) (Name s2)
        = compare s1 s2
 
instance Eq Name where
 (==) (Name s1) (Name s2)
        = s1 == s2