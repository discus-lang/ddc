
-- | Convert all `LVar` constructor labels in patterns into their equivalent
--	`LIndex` labels. This makes the Core to Sea IR translation easier.
module Core.LabelIndex
	(labelIndexGlob)
where
import Core.Plate.Trans
import DDC.Core.Exp
import DDC.Core.Glob
import DDC.Type.Data
import DDC.Var
import Data.MaybeUtil
import qualified Data.Map	as Map


-- | Convert `LVar` labels to `LIndex` labels in a glob.
labelIndexGlob
	:: Glob			-- ^ Header glob.
	-> Glob			-- ^ Module glob.
	-> Glob
	
labelIndexGlob cgHeader cgModule
 = let	lookupCtorDef :: Var -> Maybe CtorDef
	lookupCtorDef name	
	  	= takeFirstJust
			[ Map.lookup name (globDataCtors cgModule)
			, Map.lookup name (globDataCtors cgHeader) ]

   in	mapBindsOfGlob 
		(transformW $ labelIndexW lookupCtorDef) 
		cgModule
  

labelIndexW lookupCtorDef xx
 = case xx of
	WVar{}
	 ->	xx

 	WCon sp ctorName lvts	
	 -> let	(ls, vs, ts)	= unzip3 lvts
		Just ctorDef	= lookupCtorDef ctorName
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
	  
	    
