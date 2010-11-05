{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Desugar.Elaborate.Quantify
	( elabQuantifySigsInGlob)
where
import DDC.Desugar.Exp
import DDC.Desugar.Glob
import DDC.Base.SourcePos
import DDC.Type
import DDC.Var
import DDC.Main.Error
import Data.Maybe
import Data.Set			(Set)
import qualified Data.Set	as Set
import qualified Data.Map	as Map

stage	= "DDC.Desugar.Elaborate.Quantify"

-- | Add missing quantifiers to type signatures in a glob.
--   The TyCons in the tree needs to have their data defs attached so we can
--   determine what the material vars are in each sig.
-- 
--   TODO: Complain if the user writes a sig that quantifies a static variable.
--
elabQuantifySigsInGlob 
	:: Set Var	-- ^ don't quantify these variables.
	-> Glob	SourcePos
	-> (Glob SourcePos, Set Var)
	
elabQuantifySigsInGlob vsMono glob
 = let	
	-- get non-quantifiable vars from all sigs.
	(rsTop, rsData, rsClo, vsDanger)
		= staticVarsFromSigsOfGlob glob
	
	vsMono'	= Set.unions [vsMono, rsTop, rsData, rsClo, vsDanger]

	glob'	= glob 	{ globTypeSigs 	= Map.map (map (elabQuantifySig vsMono')) (globTypeSigs glob)
			, globExterns	= Map.map (elabQuantifySig vsMono') (globExterns glob) }
			
   in	(glob', vsMono')


elabQuantifySig 
	:: Set Var 	-- don't quantify these vars.
	-> Top SourcePos 
	-> Top SourcePos
	
elabQuantifySig vsMono pp
 = case pp of
	PTypeSig sp mode vs tSig
 	 -> let	tSig'	= elabQuantifySigT vsMono tSig
	    in	PTypeSig sp mode vs tSig' 

	PExtern sp v tSig mSeaType
	 -> let	tSig'	= elabQuantifySigT vsMono tSig
	    in	PExtern sp v tSig' mSeaType
	
	_ -> panic stage "elabQuantifySig: no match"

elabQuantifySigT vsMono tSig
 = let	vtsHere	= freeTVars tSig
	bksHere	= [ (BVar v, k)	| TVar k (UVar v)	<- Set.toList vtsHere
				, not $ Set.member v vsMono ]
						
   in	makeTForall_back bksHere tSig	

-- | Get the variables from type sigs in this glob that cannot be generalised,
--   either because they represent static data, are in a closure, or are dangerous.
-- 
--   TODO: collect dangerous variables.
--  	   handle cases where the user adds their own quantifiers.
--
staticVarsFromSigsOfGlob
	:: Glob SourcePos
	-> ( Set Var	-- Region vars in top-level region declarations.
	   , Set Var	-- Region vars that represent static data.
	   , Set Var	-- Region vars in closures.
	   , Set Var)	-- Dangerous vars.

staticVarsFromSigsOfGlob glob
 = 	( Set.fromList $ Map.keys $ globRegions glob

	, Set.unions
		$ map staticRsT'
		$ map (\p@PTypeSig{} -> topTypeSigType p)
		$ concat
		$ Map.elems $ globTypeSigs glob
		
	, Set.empty
	, Set.empty)
	
-- TODO: This just takes material vars, we want dangerous ones as well.
staticRsT' :: Type -> Set Var
staticRsT' tt
	= Set.fromList
	$ mapMaybe takeVarOfType 
	$ Set.toList
	$ materialRsT tt
	
	



	