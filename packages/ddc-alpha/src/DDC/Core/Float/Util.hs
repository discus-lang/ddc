{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Core.Float.Util
	( reduceEffect
	, slurpWitnessKind)
where
import DDC.Core.Float.Table
import DDC.Var
import DDC.Type
import qualified Data.Set	as Set
import Data.Set			(Set)

-- | Use information about what regions are const and what effects are pure to reduce this effect.
reduceEffect 
	:: Set Var	-- regions known to be constant.
	-> Set Var	-- types known to be deep constant.
	-> Set Var	-- effect vars known to be pure
	-> Effect -> Effect
	
reduceEffect rsConst tsConst esPure eff
 = let	eff'	= makeTSum kEffect 
		$ map (reduceEffect1 rsConst tsConst esPure)
		$ flattenTSum eff

   in {- trace 	( "reduceEffect\n"
  		% "    eff     = " % eff 	% "\n"
  		% "    eff'    = " % eff'	% "\n") $ -}
		eff'

reduceEffect1 rsConst _ esPure eff

	-- reduce reads from known constant regions
	| TApp t1 (TVar _ (UVar r))		<- eff
	, t1 == tRead
	, Set.member r rsConst
	= tPure
	
	-- reduce reads from known constant types
	| TApp t1 (TVar _ (UVar t))		<- eff
	, t1 == tDeepRead
	, Set.member t rsConst
	= tPure

	-- reduce known pure effect variables
 	| TVar kE (UVar v)			<- eff
	, kE	== kEffect
	, Set.member v esPure
	= tPure

	-- leave it 
	| otherwise
	= eff


-- | If this is a witness to constness of a region or type, or purity of an effect
--	then slurp it into the table.
slurpWitnessKind 
	:: Table -> Kind -> Table
	
slurpWitnessKind tt kk
 = case kk of
	-- const regions
 	KApp k (TVar kR (UVar r))
 	 | k    == kConst
	 , kR	== kRegion
	 -> tt { tableConstRegions 
	 		= Set.insert r (tableConstRegions tt)}
	
	-- const types
	KApp k (TVar kV (UVar t))
	 | k	== kDeepConst
	 , kV	== kValue
	 -> tt { tableConstTypes
	 		= Set.insert t (tableConstTypes tt)}

	-- pure effects
	KApp k (TVar kE (UVar e))
	 | k	== kPure
	 , kE	== kEffect
	 -> tt { tablePureEffects
	 		= Set.insert e (tablePureEffects tt) }

	-- not an interesting kind
	_	-> tt

