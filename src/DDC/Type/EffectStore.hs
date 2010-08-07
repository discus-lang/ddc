
module DDC.Type.EffectStore
	( EffectStore
	, empty
	, insert)
where
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Type.Compounds
import DDC.Type.Builtin
import DDC.Type.Kind
import DDC.Type.Exp
import DDC.Type.Crush
import DDC.Var
import Data.Set			(Set)
import qualified Data.Set	as Set

stage = "DDC.Type.EffectStore"

-- | An efficient data structure for managing effects in normalised form.
data EffectStore
	= EffectStore

	  -- | Reads on region variables. 
	{ esReads	:: Set Var
		
	  -- | Deep reads on type variables.
	, esDeepReads	:: Set Var

	  -- | Writes on region variables.
	      
	, esWrites	:: Set Var
	
	  -- | Deeps writes on type variables.
	, esDeepWrites	:: Set Var
		
	  -- | Plain effect vars.
	, esVar		:: Set Var
	
	  -- | Other effects not covered by above.
	, esOther	:: [Effect] }
	deriving (Show)


-- instance Pretty EffectStore PMode where
--	ppr	= ppr . toEffect


-- | An empty effect store
empty :: EffectStore
empty 	= EffectStore
	{ esReads	= Set.empty
	, esDeepReads	= Set.empty
	, esWrites	= Set.empty
	, esDeepWrites	= Set.empty
	, esVar		= Set.empty
	, esOther	= [] }
	
	
-- | Insert an `Effect` into an `EffectStore`.
insert :: Effect -> EffectStore -> EffectStore
insert eff es
 = case crushT eff of

	-- Vars with more-than bounds should also be annotated directly.
	TConstrain e _
	 -> insert e es
	
	TVar k (UVar v)
	 -> es { esVar	= Set.insert v (esVar es) }
	
	TVar k (UMore v _ )
	 -> es { esVar	= Set.insert v (esVar es) }

	eff'@TCon{} 
	 -> es { esOther = eff' : esOther es }
		
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
		 = es { esOther		= eff' : esOther es }
	    in	result
	
	TSum k effs
	 | isEffectKind k
	 -> foldr insert es effs
	
	_ 	-> panic stage $ vcat
			[ "insert: no match for " % eff
			, ppr $ show eff]

	
	
	
	
	
	
	

