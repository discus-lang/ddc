{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Desugar.Elaborate.Quantify
	(elabQuantifySigsInGlob)
where
import DDC.Source.Error
import DDC.Desugar.Exp
import DDC.Desugar.Glob
import DDC.Base.SourcePos
import DDC.Type
import DDC.Var
import DDC.Main.Error
import Data.Maybe
import Util
import qualified Data.Set	as Set
import qualified Data.Map	as Map

stage		= "DDC.Desugar.Elaborate.Quantify"

-- | Add missing quantifiers to type signatures in a glob.
--   The TyCons in the tree needs to have their data defs attached so we can
--   determine what the material vars are in each sig.
-- 
--   TODO: Complain if the user writes a sig that quantifies a static variable.
--
elabQuantifySigsInGlob 
	:: Set Var	-- ^ don't quantify these variables.
	-> Glob	SourcePos
	-> (Glob SourcePos, Set Var, [Error])
	
elabQuantifySigsInGlob vsMono glob
 = let	
	-- get non-quantifiable vars from all sigs.
	(rsTop, rsData, rsClo, vsDanger, errs)
		= staticVarsFromSigsOfGlob glob
	
	vsMono'	= Set.unions [vsMono, rsTop, rsData, rsClo, vsDanger]

	glob'	= glob 	{ globTypeSigs 	= Map.map (map (elabQuantifySig vsMono')) (globTypeSigs glob)
			, globExterns	= Map.map (elabQuantifySig vsMono') (globExterns glob) }
			
   in	(glob', vsMono', errs)


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
--	   emit errors for vars in sigs that have been quantified.
--
staticVarsFromSigsOfGlob
	:: Glob SourcePos
	-> ( Set Var	-- Region vars in top-level region declarations.
	   , Set Var	-- Region vars that represent material data.
	   , Set Var	-- Region vars in closures.
	   , Set Var	-- Dangerous vars.
	   , [Error] )	-- See NOTE [Quantified monomorphic vars]

staticVarsFromSigsOfGlob glob
 = let	(vssMaterial, errssMaterial)
		= unzip
		$ map (\p
			-> let	PTypeSig{}	= p
			   	Just v		= takeHead $ topTypeSigVars p
			   in	materialRegionsT v (topTypeSigType p))
		$ concat
		$ Map.elems $ globTypeSigs glob
		
   in	( Set.fromList $ Map.keys $ globRegions glob
	, Set.unions   $ vssMaterial
	, Set.empty
	, Set.empty
	, concat errssMaterial)
	

-- TODO: This just takes material vars, we want dangerous ones as well.
materialRegionsT :: Var -> Type -> (Set Var, [Error])
materialRegionsT vSig tSig
 = let	(bks, tBodyCrs)	= takeTForall tSig
   	
	-- quantified variables
	vsQuant		= Set.fromList
			$ mapMaybe (takeVarOfBind . fst) bks

	-- material variables in the type.
	vsMaterial	= Set.fromList 
			$ mapMaybe takeVarOfType 
			$ Set.toList
			$ materialRsT tBodyCrs
		
	-- Variables that have been quantified, but sadly must be
	-- monomorphic because they are material
	vsErrorMaterial	= vsQuant `Set.intersection` vsMaterial
	
   in 	( vsMaterial
	, map (ErrorQuantifiedMaterialVar vSig tSig) 
		$ Set.toList vsErrorMaterial )

	
-------------------------------------------------------------------------------

{-	NOTE [Quantified monomorphic vars]
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	The following signature is bad:
	 x :: forall %r1. Int %r1

	The variable %r1 is material in the type, meaning we get the same Int
	for every occurrence of 'x'. The type inferencer doesn't generalise 
	material vars, yet there is nothing stopping the user from writing
	bad quantifiers in their source programs. Detect this and give an error.
-}




	