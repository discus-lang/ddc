
-- Convert all LVar constructor labels in patterns into their equivalent
--	LIndex labels. This makes the Core -> Sea translation easier.
--
module Core.LabelIndex
	(labelIndexTree)
where
import Core.Exp
import Core.Plate.Trans
import DDC.Var
import qualified Data.Map	as Map
import Data.Map			(Map)


-- | Convert LVar labels to LIndex labels in a tree.
labelIndexTree
	:: Map Var CtorDef	-- ^ map of data constructor names to definition
	-> Tree	
	-> Tree
	
labelIndexTree
	mapCtorDef
	tree
 = 	transformW (labelIndexW mapCtorDef) tree
  
labelIndexW mapCtorDefs xx
 = case xx of
	WVar{}
	 ->	xx

 	WCon sp ctorName lvts	
	 -> let	(ls, vs, ts)	= unzip3 lvts
		Just ctorDef	= Map.lookup ctorName mapCtorDefs
	 	ls'		= map (convertLabel ctorDef) ls
		lvts'		= zip3 ls' vs ts
	    in	WCon sp ctorName lvts'
	 
	WLit{}
	 ->	xx
 
 
-- | Given the appropriate constructor definition,
--	Convert LVar labels to LIndex lables.
convertLabel 
	:: CtorDef
	-> Label
	-> Label
	
convertLabel ctor label
 = case label of
 	LIndex i	-> label
	
	LVar v
	 -> let	Just ix	= Map.lookup v (ctorDefFields ctor)
	    in	LIndex ix
	  
	    
