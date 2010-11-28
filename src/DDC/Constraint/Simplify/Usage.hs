{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Analyse how constrained variables are used.
module DDC.Constraint.Simplify.Usage
	( UseSide	(..)
	, Usage		(..)
	, UseMap	(..)
	, slurpUsage
	, singleton
	, usedIsWanted
	, usedJustOnceInEq)
where
import DDC.Constraint.Util
import DDC.Constraint.Exp
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Type
import Data.Monoid
import Data.Map			(Map)
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import qualified Data.Foldable	as Seq

stage	= "DDC.Constraint.Simplify.Usage"

-- Usage ------------------------------------------------------------------------------------------
-- | What side of a constraint a variable appears on.
data UseSide
	= OnLeft
	| OnRight
	deriving (Eq, Ord, Show)

	
-- | How a constrained type variable is used.
data Usage
	-- | Is wanted by the Desugar -> Core translation.
	= UsedWanted

	-- | Appears in an eq constraint.
	| UsedEq UseSide
	
	-- | Appears in a more-than constraint.
	| UsedMore UseSide
	
	-- | Appears in an inst constraint.
	| UsedInst UseSide

	-- | Appears in the binding vars list in a branch.
	| UsedBranchBind

	-- | Used in a projection constraint
	| UsedProject
	
	-- | Is generalised
	| UsedGen
	deriving (Eq, Ord, Show)

instance Pretty Usage PMode where
	ppr uu	= ppr $ show uu


-- UseMap -----------------------------------------------------------------------------------------
-- | Maps type vars to how many times each sort of usage appears.
data UseMap
	= UseMap (Map Type (Map Usage Int))


instance Monoid UseMap where
 mempty	
  	= UseMap $ Map.empty

 mappend (UseMap mx) (UseMap my)
  	= UseMap
	$ Map.unionWith (Map.unionWith (+)) mx my


instance Pretty UseMap PMode where
	ppr (UseMap mp)
	 = vcat [padL 30 v %% use 
			| (v, use) <- Map.toList mp]


singleton :: Usage -> Type -> UseMap
singleton = flip singleton'

singleton' :: Type -> Usage -> UseMap
singleton' t@(TVar k _) usage
	-- we don't care about constraints on region vars.
	| isRegionKind k	= mempty
	| otherwise		= UseMap $ Map.singleton t (Map.singleton usage 1)

singleton' t1 _
	= panic stage 
	$  "singleton: only type vars can be used on the left of a constraint"
	%! "    offending type = " % t1


usedTVars :: Usage -> Type -> UseMap
usedTVars = flip usedTVars'

usedTVars'  :: Type -> Usage -> UseMap
usedTVars' t usage
	= mconcat 
	$ map (singleton usage)
	$ Set.toList 
	$ freeTVars t


-- | Var is wanted by the Desugar -> Core transform.
usedIsWanted :: UseMap -> Type -> Bool
usedIsWanted (UseMap mp) t1
	= maybe False
		(Map.member UsedWanted)
		(Map.lookup t1 mp)
		

-- | Var is only used once, in an eq constraint.
usedJustOnceInEq :: UseMap -> Type -> Bool
usedJustOnceInEq (UseMap mp) t1
 = let	hasSize1 mm = case Map.toList mm of 
			[_]	-> True
			_	-> False
			
   in	maybe	False
		(\used ->  (hasSize1 used)
			&& ( (Map.lookup (UsedEq OnRight) used == Just 1)
		          || (Map.lookup (UsedEq OnLeft)  used == Just 1)))
		(Map.lookup t1 mp)


-- slurpUsage -------------------------------------------------------------------------------------
-- | Slurps usage information from a constraint tree.
--	The result maps type vars to how many time each sort of usage appears.
slurpUsage :: CTree -> UseMap
slurpUsage cc
 = case cc of
	CBranch bs subs
	 -> mconcat
		$ slurpUsageFromCBind bs
		: (map slurpUsage $ Seq.toList subs)
		
	CEq _ t1@TVar{} t2
	 -> mappend
		(singleton (UsedEq OnLeft)  t1)
		(usedTVars (UsedEq OnRight) t2)

	CMore _ t1 t2
	 -> mappend
		(singleton (UsedMore OnLeft)  t1)
		(usedTVars (UsedMore OnRight) t2)

	CProject _ _ _ t1 t2
	 -> mappend
		(usedTVars UsedProject t1)
		(usedTVars UsedProject t2)

	CInst _ t1 t2
	 -> mappend
		(singleton (UsedInst OnLeft)  $ TVar kValue (UVar t1))
		(singleton (UsedInst OnRight) $ TVar kValue (UVar t2))
		
	CGen _ t1
	 -> 	singleton UsedGen t1

	_ -> panic stage $ "slurpUsage: no match"

		
slurpUsageFromCBind :: CBind -> UseMap
slurpUsageFromCBind cbind
	= mconcat
	$ map (singleton UsedBranchBind)
	$ map (\v -> TVar kValue (UVar v))
	$ takeCBindVs cbind

