{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Constraint.Simplify.Collect
	( Table (..)
	, collect)
where
import DDC.Constraint.Exp
import DDC.Type
import DDC.Var
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
	, tableMore	:: Map Type Type }

instance Monoid Table where
 mempty	= Table Map.empty Map.empty

 mappend (Table eq1 more1) (Table eq2 more2)
	= Table (Map.union eq1 eq2) (Map.union more1 more2)

singleEq   :: Type -> Type -> Table
singleEq t1 t2 	= tableMore `seq` Table (Map.singleton t1 t2) Map.empty 

-- singleMore :: Type -> Type -> Table
-- singleMore t1 t2 = Table Map.empty (Map.singleton t1 t2)



-- | Collect up a table of bindings that can be safely inlined.
collect :: Set Var
	-> CTree
	-> Table

collect wanted cc
 = let	doNotWant (TVar _ (UVar v))	= not $ Set.member v wanted
	doNotWant _			= True

   in case cc of
	CBranch{}
	 -> mconcat $ map (collect wanted) $ Seq.toList $ branchSub cc

	CEqs _ 	[t1, t2@TVar{}]
	 | doNotWant t1			-> singleEq t1 t2

	CEq _	t1 t2@TVar{}	
	 | doNotWant t1 		-> singleEq t1 t2

	_				-> mempty
