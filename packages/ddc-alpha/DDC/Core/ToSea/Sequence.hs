{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- | Determine the depdendency order for CAFs.
--   In the Sea code we need to initialise them in order, 
--   as we don't support recursive ones.
--
--   TODO: Dont' slurp free vars if not a CAF, too slow.
--
module DDC.Core.ToSea.Sequence
	(slurpCafInitSequence)
where
import Core.Util
import Core.Plate.FreeVars
import Util.Graph.Deps
import Util
import DDC.Core.Exp
import DDC.Core.Glob
import DDC.Var
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import qualified Shared.VarUtil	as Var


-- | For the CAFs in a tree, give a dependency ordering suitable for initialisation.
--	Left  [Var] 	mututally recursive CAF vars (error)
--	Right [Var] 	valid sequential dependency order
slurpCafInitSequence 
	:: Glob 
	-> Either [Var] [Var]

slurpCafInitSequence cgSource
 = let	psBinds	= Map.elems $ globBind cgSource
	
	-- Slurp out the list of variables referenced by each top level binding.
	deps 	:: Map Var [Var]
 	deps	= Map.map slurpSuperDepsOfPBind
		$ globBind cgSource
		
	-- Work out which top level bindings are cafs.
	cafVars	= [v 	| p@(PBind v _) <- psBinds
			, isCafP p ]
			
	-- Check that the cafs are not recursively defined,
	--	our Sea level implementation does not handle this.
	recCafs		= [v	| v		<- cafVars
				, Set.member v (graphReachable1_nr deps v) ]

   in	if not $ isNil recCafs
	  then Left recCafs
	  else sequenceCafsTree2 deps cafVars
	
sequenceCafsTree2 deps cafVars
 = let 	-- Determine an appropriate initialisation order for cafs
	--	we can't call a caf if it references other which
	--	are not yet initialisd.
	initSeq	= [v	| v	<- graphSequence deps Set.empty cafVars
			, elem v cafVars]

  in	Right initSeq
	

-- | Slurps out the list of value variables which are free in
--   each top level binding in a tree.
slurpSuperDepsOfPBind :: Top -> [Var]
slurpSuperDepsOfPBind pp
 = case pp of
 	PBind _ x	-> slurpDepsX x
	_ 		-> error "Core.Sequence: slurpSuperDepsOfPBind not PBind"

slurpDepsX xx
 	= [ v	| v <- Set.toList $ freeVars xx
		, not $ Var.isCtorName v
		, varNameSpace v == NameValue ]
			
