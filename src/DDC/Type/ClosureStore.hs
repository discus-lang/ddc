
module DDC.Type.ClosureStore 
	( ClosureStore
	, empty
	, insert
	, union
	, unions
	, mask
	, toClosure
	, fromClosure)
where
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Kind
import DDC.Type.Builtin
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Var
import DDC.Type.Pretty			()
import Data.Map				(Map)
import Data.Set				(Set)
import qualified Data.Map		as Map
import qualified Data.Set		as Set

stage = "DDC.Type.ClosureStore"

-- | An efficient data structure for managing sets of closure types.
data ClosureStore
	= ClosureStore
	{ csFreeRs	:: Map Var (Set Var)
	, csFreeTs	:: Map Var (Set Var)
	, csFreeCs	:: Map Var (Set Var)
	, csVar		:: Set Var }
	deriving Show

instance Pretty ClosureStore PMode where
	ppr	= ppr . toClosure

-- | An empty ClosureStore
empty :: ClosureStore
empty 	= ClosureStore
	{ csFreeRs	= Map.empty
	, csFreeTs	= Map.empty
	, csFreeCs	= Map.empty
	, csVar		= Set.empty }


-- | Insert a `Closure` into a `ClosureStore`
insert :: Closure -> ClosureStore -> ClosureStore
insert clo cs
 = case clo of
	TVar k (UVar v)	
	 -> cs { csVar  = Set.insert v (csVar cs) }

	TApp{}
	 | Just (v1, TVar k (UVar v2)) <- takeTFree clo
	 , isRegionKind k
	 -> cs { csFreeRs = Map.unionWith 
				Set.union
				(csFreeRs cs)
				(Map.singleton v1 (Set.singleton v2)) }

	 | Just (v1, TVar k (UVar v2)) <- takeTFree clo
	 , isValueKind k
	 -> cs { csFreeTs = Map.unionWith 
				Set.union
				(csFreeTs cs)
				(Map.singleton v1 (Set.singleton v2)) }

	 | Just (v1, TVar k (UVar v2)) <- takeTFree clo
	 , isClosureKind k
	 -> cs { csFreeCs = Map.unionWith 
				Set.union
				(csFreeCs cs)
				(Map.singleton v1 (Set.singleton v2)) }
	
	TSum k clos
	 | isClosureKind k
	 -> foldr insert cs clos
	
	_ 	-> panic stage
		$  "insert: no match for " % clo

	
-- | Union two ClosureStores
union :: ClosureStore -> ClosureStore -> ClosureStore
union cs1 cs2
	= ClosureStore
	{ csFreeRs	= Map.unionWith Set.union (csFreeRs cs1) (csFreeRs cs2)
	, csFreeTs	= Map.unionWith Set.union (csFreeTs cs1) (csFreeTs cs2)
	, csFreeCs	= Map.unionWith Set.union (csFreeCs cs1) (csFreeCs cs2)
	, csVar		= Set.union (csVar cs1) (csVar cs2) }

-- | Union several closures
unions :: [ClosureStore] -> ClosureStore
unions	= foldr union empty

-- | Mask all the parts of a closure due to a specific value variable.
mask :: Var -> ClosureStore -> ClosureStore
mask vv cs
	= ClosureStore
	{ csFreeRs	= Map.delete vv (csFreeRs cs)
	, csFreeTs	= Map.delete vv (csFreeTs cs)
	, csFreeCs	= Map.delete vv (csFreeCs cs)
	, csVar		= csVar cs }


-- | Convert a `ClosureStore` to a regular `Closure`.
toClosure :: ClosureStore -> Closure
toClosure cs
 	= makeTSum kClosure
	$  [TVar kClosure (UVar v) 
		| v <- Set.toList (csVar cs) ]

	++ concat 
		[ map (\vr  -> makeTFree v (TVar kRegion (UVar vr))) $ Set.toList vrs
			| (v, vrs)	<- Map.toList (csFreeRs cs) ]

	++ concat 
		[ map (\vr  -> makeTFree v (TVar kValue (UVar vr))) $ Set.toList vrs
			| (v, vrs)	<- Map.toList (csFreeTs cs) ]

	++ concat 
		[ map (\vr  -> makeTFree v (TVar kClosure (UVar vr))) $ Set.toList vrs
			| (v, vrs)	<- Map.toList (csFreeCs cs) ]


-- | Convet a `Closure` to a `ClosureStore`
fromClosure :: Closure -> ClosureStore
fromClosure clo
	= insert clo empty

