{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Constraint.Simplify.Collect
	( Table (..)
	, collect)
where
import DDC.Constraint.Simplify.Usage
import DDC.Constraint.Exp
import DDC.Type
import Data.Monoid
import Data.Set				(Set)
import Data.Map				(Map)
import qualified Data.Foldable		as Seq
import qualified Data.Set		as Set
import qualified Data.Map		as Map


-- Collect ----------------------------------------------------------------------------------------
-- | A table of bindings that can be safely inlined.
data Table
	= Table 
	{ tableEq	:: Map Type Type 
	, tableMore	:: Map Type Type
	, tableNoInline	:: Set Type }

instance Monoid Table where
 mempty	= Table Map.empty Map.empty Set.empty

 mappend (Table eq1 more1 noInline1) (Table eq2 more2 noInline2)
  = let	noInline	= Set.union noInline1 noInline2 
	eq		= foldr Map.delete (Map.union eq1   eq2)   $ Set.toList noInline
	more		= foldr Map.delete (Map.union more1 more2) $ Set.toList noInline
    in	Table eq more noInline
		
		
singleNoInline :: Type -> Table
singleNoInline t1 = Table Map.empty Map.empty (Set.singleton t1)	
		
singleEq   :: Type -> Type -> Table
singleEq t1 t2 	= tableMore `seq` Table (Map.singleton t1 t2) Map.empty Set.empty

-- singleMore :: Type -> Type -> Table
-- singleMore t1 t2 = Table Map.empty (Map.singleton t1 t2)


-- | Collect up a table of bindings that can be safely inlined.
collect :: UseMap
	-> CTree
	-> Table

collect usage cc
 = let doNotWant t 
		= not $ elem UsedWanted $ map fst
		$ lookupUsage t usage

   in case cc of
	CBranch{}
	 -> mconcat $ map (collect usage) $ Seq.toList $ branchSub cc

	-- inline  v1 = v2  renames.
	CEq _   t1@TVar{} t2@TVar{}
	 | doNotWant t2
	 , [(UsedEq OnLeft, 1), (UsedEq OnRight, 1)] 
		<- lookupUsage t1 usage
	 -> singleEq t1 t2

	 | doNotWant t1
	 , [(UsedEq OnLeft, 1), (UsedEq OnRight, 1)] 
		<- lookupUsage t2 usage
	 -> singleEq t2 t1


	CInst _ v _			-> singleNoInline (TVar kValue (UVar v))

	_				-> mempty
