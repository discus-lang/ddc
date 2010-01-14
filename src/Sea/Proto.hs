
-- | Add prototypes for each supercombinator.
module Sea.Proto
	(addSuperProtosTree)

where
import Sea.Exp
import Sea.Util
import Util

-- | Add prototypes for each supercombinator in this tree.
addSuperProtosTree 
	:: Tree () 
	-> Tree ()

addSuperProtosTree tree
 = concatMap
	(\p -> case p of
			PSuper v args r ss	
			 -> [ PProto v (map snd args) r
			    , p]
			    
			_ -> [p])
	$ tree
