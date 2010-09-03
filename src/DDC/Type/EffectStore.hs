
module DDC.Type.EffectStore
	( EffectStore
	, pure
	, union
	, unions
	, insert
	, toEffect
	, fromEffect
	, maskReadWritesNotOn
	, maskReadWritesOn)
where
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Type.Compounds
import DDC.Type.Builtin
import DDC.Type.Kind
import DDC.Type.Exp
import DDC.Type.Operators.Crush
import DDC.Var
import Data.Set			(Set)
import qualified Data.Set	as Set

stage = "DDC.Type.EffectStore"

-- | An efficient data structure for managing effects in normalised form.
data EffectStore
	= EffectStore 
	{  -- | Reads on region variables.
	  esReads	:: Set Var
		
	  -- | Deep reads on type variables.
	, esDeepReads	:: Set Var

	  -- | Writes on region variables.
	      
	, esWrites	:: Set Var
	
	  -- | Deeps writes on type variables.
	, esDeepWrites	:: Set Var
		
	  -- | Plain effect vars.
	, esVars	:: Set Var
	
	  -- | Other effects not covered by above.
	, esOthers	:: [Effect] }
	deriving (Show)


instance Pretty EffectStore PMode where
	ppr 	= ppr . toEffect


-- | An empty effect store
pure :: EffectStore
pure 	= EffectStore
	{ esReads	= Set.empty
	, esDeepReads	= Set.empty
	, esWrites	= Set.empty
	, esDeepWrites	= Set.empty
	, esVars	= Set.empty
	, esOthers	= [] }
	
	
-- | Insert an `Effect` into an `EffectStore`.
insert :: Effect -> EffectStore -> EffectStore
insert eff es
 = case crushT eff of

	-- Vars with more-than bounds should also be annotated directly.
	TConstrain e _
	 -> insert e es
	
	TVar k (UVar v)
	 -> es { esVars	= Set.insert v (esVars es) }
	
	TVar k (UMore v _ )
	 -> es { esVars	= Set.insert v (esVars es) }

	eff'@TCon{} 
	 -> es { esOthers = eff' : esOthers es }
		
	eff'@TApp{}
	 | [t1, TVar k b]	<- takeTApps eff'
	 , Just v		<- takeVarOfBound b
	 -> let	result
		 | t1 == tRead
		 , isRegionKind k	
		 = es { esReads		= Set.insert v (esReads es) }

		 | t1 == tDeepRead
		 , isValueKind k	
		 = es { esDeepReads	= Set.insert v (esDeepReads es) }

		 | t1 == tWrite
		 , isRegionKind k	
		 = es { esWrites	= Set.insert v (esWrites es) }

		 | t1 == tDeepWrite
		 , isValueKind k	
		 = es { esDeepWrites	= Set.insert v (esDeepWrites es) }
		
		 | otherwise
		 = es { esOthers	= eff' : esOthers es }
	    in	result
	
	TSum k effs
	 | isEffectKind k
	 -> foldr insert es effs
	
	_ 	-> panic stage $ vcat
			[ "insert: no match for " % eff
			, ppr $ show eff]


-- | Union two `EffectStore`s
union :: EffectStore -> EffectStore -> EffectStore
union es1 es2
	= EffectStore
	{ esReads		= Set.union (esReads es1)	(esReads es2)
	, esDeepReads		= Set.union (esDeepReads es1)	(esDeepWrites es2)
	, esWrites		= Set.union (esWrites es1)	(esWrites es2)
	, esDeepWrites		= Set.union (esDeepWrites es1)	(esDeepWrites es2)
	, esVars		= Set.union (esVars es1)	(esVars es2)
	, esOthers		= esOthers es1 ++ esOthers es2
	}


-- | Union several `EffectStores`.
unions :: [EffectStore] -> EffectStore
unions	= foldr union pure


-- | Convert an `EffectStore` to a regular `Effect`.
toEffect :: EffectStore -> Effect
toEffect es
	= makeTSum kEffect
	$  [ makeTApp tRead      [TVar kRegion (UVar v)] | v <- Set.toList $ esReads      es ]
	++ [ makeTApp tDeepRead  [TVar kValue  (UVar v)] | v <- Set.toList $ esDeepReads  es ]
	++ [ makeTApp tWrite     [TVar kRegion (UVar v)] | v <- Set.toList $ esWrites     es ]
	++ [ makeTApp tDeepWrite [TVar kValue  (UVar v)] | v <- Set.toList $ esDeepWrites es ]
	++ [ TVar kEffect (UVar v)			 | v <- Set.toList $ esVars       es ]
	++ esOthers es
	

-- | Convert an `Effect` into a `EffectStore`
fromEffect :: Effect -> EffectStore
fromEffect eff
	= insert eff pure


-- | Mask Read and Write effects on regions that are not on the region variables in this set.
maskReadWritesNotOn :: Set Var -> EffectStore -> EffectStore
maskReadWritesNotOn vsVisible es
	= es
	{ esReads	= Set.intersection (esReads  es) vsVisible
	, esWrites	= Set.intersection (esWrites es) vsVisible }
	

-- | Mask Read and Write effects on this region variable.
maskReadWritesOn :: Var -> EffectStore -> EffectStore
maskReadWritesOn v es
	| varNameSpace v == NameRegion
	= es
	{ esReads	= Set.delete v (esReads  es)
	, esWrites	= Set.delete v (esWrites es) }
	
