{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# OPTIONS -Wnot #-}
-- | Analyse how constrained variables are used.
module DDC.Constraint.Simplify.Usage
	( UseSide	(..)
	, Usage		(..)
	, UseMap	(..)
	, emptyUsage
	, slurpUsage
	, lookupUsage
	, addUsage
	, usedIsWanted)
where
import DDC.Constraint.Util
import DDC.Constraint.Exp
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Type
import DDC.Var
import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.Hashable
import Data.HashTable		(HashTable)
import Data.Map			(Map)
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import qualified Data.Foldable	as Seq
import qualified Data.HashTable	as Hash

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
	= UseMap 
	{ useTable	:: HashTable Var (Map Usage Int) }


-- | Empty usage map
emptyUsage :: IO UseMap 
emptyUsage 
 = do	table	<- Hash.new (==) hash
	return	$ UseMap table
	

-- | Add a single usage to the use map.
--   The Type must be a TVar.
addUsage :: UseMap -> Usage -> Type -> IO ()
addUsage used@(UseMap mp) usage (TVar _ (UVar v))
 = do	-- get the usages we already have for this var.
	val	<- liftM (fromMaybe Map.empty) 
		$  Hash.lookup mp v
	
	-- update the table with the new usage information.
	Hash.update mp v
		$ Map.unionWith (+) val
		$ Map.singleton usage 1

	return ()

-- | Add all the free variables of this type as a particular usage
addUsageFree :: UseMap -> Usage -> Type -> IO ()
addUsageFree used usage t
	= mapM_ (addUsage used usage) 
	$ Set.toList
	$ freeTVars t

	
-- | Lookup the usage for some type.
--   The type must be a TVar.
lookupUsage :: UseMap -> Type -> IO (Map Usage Int)
lookupUsage (UseMap mp) (TVar _ (UVar v))
  	= liftM (fromMaybe Map.empty)
	$ Hash.lookup mp v
	

-- | Check whether a type var is wanted by the Desugar -> Core transform.
usedIsWanted :: UseMap -> Type -> IO Bool
usedIsWanted (UseMap mp) (TVar _ (UVar v))
 = do	mUsage	<- Hash.lookup mp v
	case mUsage of
	 Nothing	-> return $ False
	 Just uses	-> return $ Map.member UsedWanted uses


-- | Var is only used once, in an eq constraint.
-- usedJustOnceInEq :: UseMap -> Type -> Bool
-- usedJustOnceInEq (UseMap mp) t1
--	= undefined
{-
 = let	hasSize1 mm = case Map.toList mm of 
			[_]	-> True
			_	-> False
			
   in	maybe	False
		(\used ->  (hasSize1 used)
			&& ( (Map.lookup (UsedEq OnRight) used == Just 1)
		          || (Map.lookup (UsedEq OnLeft)  used == Just 1)))
		(Map.lookup t1 mp)
-}

-- slurpUsage -------------------------------------------------------------------------------------
-- | Slurp usage information from a constraint tree into the given usage map.
slurpUsage :: UseMap -> CTree -> IO ()
slurpUsage uses cc
 = case cc of
	CBranch binds subs
	 -> do	slurpUsageFromCBind uses binds
		mapM_ (slurpUsage uses) $ Seq.toList subs
		
		
	CEq _ t1@TVar{} t2
	 -> do	addUsage     uses (UsedEq OnLeft)    t1
		addUsageFree uses (UsedEq OnRight)   t2
		

	CMore _ t1 t2
	 -> do	addUsage     uses (UsedMore OnLeft)  t1
		addUsageFree uses (UsedMore OnRight) t2

	CProject _ _ _ t1 t2
	 -> do	addUsageFree uses UsedProject t1
		addUsageFree uses UsedProject t2
		
	CInst _ t1 t2
	 -> do	addUsage     uses (UsedInst OnLeft)  $ TVar kValue (UVar t1)
		addUsage     uses (UsedInst OnRight) $ TVar kValue (UVar t2)
			
	CGen _ t1
	 -> do	addUsage     uses UsedGen t1

	_ -> panic stage $ "slurpUsage: no match"

		
slurpUsageFromCBind :: UseMap -> CBind -> IO ()
slurpUsageFromCBind uses cbind
 =	mapM_ (addUsage uses UsedBranchBind) 
		$ map (\v -> TVar kValue (UVar v))
		$ takeCBindVs cbind

