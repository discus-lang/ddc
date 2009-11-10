-----
-- Core.LabelIndex
--	Converts all LVar constructor labels into their equivalent LIndex labels.
--	This is done to make the Core -> Sea translation easier.
--
--	A table of constructor definitions is used to do make the var -> index 
--	translation.
--
module Core.LabelIndex
	( labelIndexTree )
where

import qualified Data.Map	as Map
import Data.Map			(Map)

import Util
import Core.Exp
import Core.Plate.Trans


labelIndexTree
	:: Map Var CtorDef
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

 	WCon spos ctorName lvts	
	 -> let	(ls, vs, ts)	= unzip3 lvts
		Just ctorDef	= Map.lookup ctorName mapCtorDefs
	 	ls'		= map (convertLabel ctorDef) ls
		lvts'		= zip3 ls' vs ts
	    in	WCon spos ctorName lvts'
	 
	WLit{}
	 ->	xx
 
 
convertLabel 
	:: CtorDef
	-> Label
	-> Label
	
convertLabel (CtorDef v fields) label
 = case label of
 	LIndex i	-> label
	
	LVar v
	 -> let	Just (_, ix)	
	 		= find (\(f, i) -> dLabel f == Just v) 
			$ zip fields [0..]
			
	    in	LIndex ix
	  
	    
