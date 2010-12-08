{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Type.Operators.MaskLocal
	(maskLocalT)
where
import Util
import Shared.VarPrim
import DDC.Type.Exp
import DDC.Type.Predicates
import DDC.Type.Compounds
import DDC.Type.Kind
import DDC.Type.Builtin
import DDC.Type.Pretty	()
import qualified Data.Set	as Set


-- | Mask effects on local regions.
--   
--   We can also erase Const/Mutable Lazy/Direct constraints because these will be
--   fulfilled by the letregion construct used to locally create the region.
--
maskLocalT
	:: Set Type	-- ^ Set of visible TVars in the type.
			--   Compute this with `visibleRsT` from "DDC.Type.Collect.Visible"
	-> Type
	-> Type
	
maskLocalT tsVis tt
 = case tt of
	TForall  b k t1	
	 -> TForall b k (maskLocalT tsVis t1)

	TConstrain t1 crs
	 -> TConstrain t1 
	  $ constraintsOfFetters
	  $ catMaybes 
	  $ map (maskLocalF tsVis)
	  $ fettersOfConstraints crs

	_ -> tt


-- | Erase read effects to regions not in this list.
--	also erase Const, Mutable, Lazy, Direct constraints on regions not in the list
maskLocalF 
	:: Set Type	-- ^ Set of visible TVars in the type.
	-> Fetter 
	-> Maybe Fetter

maskLocalF tsVis (FWhere t1 t2)
	| isEffect t1
	= Just $ FWhere t1 (maskLocalEff tsVis t2)

maskLocalF tsVis (FMore t1 t2)
	| isEffect t1
	= Just $ FMore  t1 (maskLocalEff tsVis t2)

maskLocalF tsVis (FConstraint v [tR])
	| elem v [primConst, primMutable, primLazy, primDirect]
	, isTClass tR || isSomeTVar tR
	, not $ Set.member tR tsVis
	= Nothing
	
maskLocalF _ f	
	= Just f


-- | Erase read effects to regions not in this list.
maskLocalEff
	:: Set Type	-- ^ Set of visible vars in the type.
	-> Effect
	-> Effect

maskLocalEff  tsVis eff
	= makeTSum kEffect 
	$ mapMaybe (maskLocalEff' tsVis) 
	$ flattenTSum eff 
	
maskLocalEff' tsVis eff
	| TApp t1 tR	<- eff
	, elem t1 [tRead, tWrite]
	, isTClass tR || isSomeTVar tR
	, not $ Set.member tR tsVis
	= Nothing
	
	| otherwise
	= Just eff


