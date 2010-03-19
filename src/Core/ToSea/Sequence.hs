{-# OPTIONS -fwarn-unused-imports #-}

-- | Determine the depdendency order for CAFs.
--	In the Sea code we need to initialise them in order, 
--	as we don't support recursive ones.
--
module Core.ToSea.Sequence
	(slurpCafInitSequence)
where
import Core.Exp
import Core.Glob
import Core.Util
import Core.Plate.FreeVars
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import qualified Shared.Var	as Var
import qualified Shared.VarUtil	as Var
import Shared.Var		(NameSpace(..))
import Util.Graph.Deps
import Util

-- | For the CAFs in a tree, give a dependency ordering suitable for initialisation.
--	Left [Var] 	mututally recursive CAF vars (error)
--	Right [Var] 	valid sequential dependency order
--
slurpCafInitSequence 
	:: Glob 
	-> IO (Either [Var] [Var])

slurpCafInitSequence cgSource
 = let	psBinds	= Map.elems $ globBind cgSource
	
	-- Slurp out the list of variables referenced by each top level binding.
	deps 	:: Map Var [Var]
 	deps	= Map.map slurpSuperDepsOfPBind
		$ globBind cgSource
		
	-- Work out which top level bindings are cafs.
	cafVars		= [v 	| p@(PBind v x) <- psBinds
				, isCafP p ]
			
	-- Check that the cafs are not recursively defined,
	--	our Sea level implementation does not handle this.
	recCafs		= [v	| v		<- cafVars
				, Set.member v (graphReachable deps [v]) ]

   in	if not $ isNil recCafs
	  then return $ Left recCafs
	  else sequenceCafsTree2 deps cafVars
	
sequenceCafsTree2 deps cafVars
 = let
 	-- Work out an appropriate initialisation order for cafs
	--	we can't call a caf if it references other which
	--	are not yet initialisd.
	initSeq	= [v	| v	<- graphSequence deps Set.empty cafVars
			, elem v cafVars]

  in	return $ Right initSeq
	
-- !! TODO don't slurp free vars if not a caf -- too slow.
-- !! combine with isCafP / cafVars


-- | Slurps out the list of value variables which are free in
--   each top level binding in a tree.
slurpSuperDepsOfPBind :: Top -> [Var]
slurpSuperDepsOfPBind pp
 = case pp of
 	PBind v x	-> slurpDepsX x
	_ 		-> error "Core.Sequence: slurpSuperDepsOfPBind not PBind"

slurpDepsX xx
 	= [ v	| v <- Set.toList $ freeVars xx
		, not $ Var.isCtorName v
		, Var.nameSpace v == NameValue ]
			
