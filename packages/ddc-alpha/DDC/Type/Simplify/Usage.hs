{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

--- | Tracking how type variables are used in constraints.
module DDC.Type.Simplify.Usage
	( Usage 	(..)
	, KindEffClo	(..)
	, UseSide	(..)
	, UseMap	(..)
	, emptyUsage
	, addUsage
	, addUsageFree
	, lookupUsage
	, usedIsWanted
	, slurpUsageCrs)
where
import DDC.Type.Compounds
import DDC.Type.Collect
import DDC.Type.Exp
import DDC.Main.Pretty
import DDC.Main.Error
import Data.Hashable
import Data.HashTable		(HashTable)
import Data.Map			(Map)
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import qualified Data.HashTable	as Hash
import Control.Monad
import Data.Maybe

stage	= "DDC.Type.Simplify.Usage"

-- KindEffClo -------------------------------------------------------------------------------------
-- | If a constraint value type variable's right occurrences are only in more-than constraints
--   for closures then we know they're going to be trimmed as components of a closure.
data KindEffClo
	= KindEff
	| KindClo
	deriving (Eq, Ord, Show)

takeEffCloOfKind :: Kind -> Maybe KindEffClo
takeEffCloOfKind kk 
 = case kk of
	KCon KiConEffect  _	-> Just KindEff
	KCon KiConClosure _	-> Just KindClo
	_			-> Nothing
	

-- Usage ------------------------------------------------------------------------------------------
-- | What side of a constraint a variable appears on.
data UseSide
	= OnLeft
	| OnRight
	deriving (Eq, Ord, Show)


data Usage
	-- | Variable is wanted in the result, so we shouldn't inline it completely.
	= UsedWanted
	
	-- | Used in an equality constraint.
	| UsedEq   UseSide
	
	-- | Used in a more-than constraint of this kind.
	| UsedMore UseSide KindEffClo

	-- | Used in a type-class predicate
	| UsedPred
	deriving (Eq, Ord, Show)

	
instance Pretty Usage PMode where
	ppr uu 	= ppr $ show uu


-- UseMap -----------------------------------------------------------------------------------------
-- | Maps type vars to how many times each sort of usage appears.
data UseMap
	= UseMap 
	{ useTable	:: HashTable Bound (Kind, Map Usage Int) }


-- | Empty usage map
emptyUsage :: IO UseMap 
emptyUsage 
 = do	table	<- Hash.new (==) hash
	return	$ UseMap table
	

-- | Add a single usage to the use map.
addUsage :: UseMap -> Usage -> Bound -> Kind -> IO ()
addUsage (UseMap mp) usage u kind
 = do	-- get the usages we already have for this var.
	(k, val)	<- liftM (fromMaybe (kind, Map.empty))
			$  Hash.lookup mp u
	
	-- update the table with the new usage information.
	Hash.update mp u
		(k, Map.unionWith (+) val (Map.singleton usage 1))

	return ()


-- | Add all the free variables of this type as a particular usage
addUsageFree :: UseMap -> Usage -> Type -> IO ()
addUsageFree used usage t
 = do	let Just uks = sequence $ map takeBoundKindOfType $ Set.toList $ freeTVars t
	mapM_ (uncurry $ addUsage used usage) uks
	
	
-- | Lookup the usage for some type.
--   The type must be a TVar.
lookupUsage :: UseMap -> Bound -> IO (Map Usage Int)
lookupUsage (UseMap mp) u
  	= liftM (fromMaybe Map.empty)
	$ liftM (liftM snd)
	$ Hash.lookup mp u
	

-- | Check whether a type var is wanted by the Desugar -> Core transform.
usedIsWanted :: UseMap -> Bound -> IO Bool
usedIsWanted (UseMap mp) u
 = do	mUsage	<- Hash.lookup mp u
	case mUsage of
	 Nothing	-> return $ False
	 Just (_, uses)	-> return $ Map.member UsedWanted uses
	

-- slurpUsage -------------------------------------------------------------------------------------
-- | Slurp usage information from some constraints into the given usage map.
slurpUsageCrs :: UseMap -> Constraints -> IO ()
slurpUsageCrs uses (Constraints crsEq crsMore crsOther)
 = do	mapM_ (uncurry $ slurpUsageEq   uses) $ Map.toList crsEq
	mapM_ (uncurry $ slurpUsageMore uses) $ Map.toList crsMore
	mapM_ (slurpUsageOther uses) crsOther


slurpUsageEq :: UseMap -> Type -> Type -> IO ()
slurpUsageEq uses (TVar k u) t2
 = do	addUsage     uses (UsedEq OnLeft)  u k
 	addUsageFree uses (UsedEq OnRight) t2

slurpUsageEq _ _ _
	= panic stage $ "slurpUsageEq: no match"
	

slurpUsageMore :: UseMap -> Type -> Type -> IO ()
slurpUsageMore uses (TVar k u) t2
 = do	let Just kEC	= takeEffCloOfKind k
	addUsage     uses (UsedMore OnLeft  kEC) u k
 	addUsageFree uses (UsedMore OnRight kEC) t2

slurpUsageMore _ _ _
	= panic stage $ "slurpUsageEq: no match"


slurpUsageOther :: UseMap -> Fetter -> IO ()
slurpUsageOther uses ff
 = case ff of
	FConstraint _ ts
	 -> mapM_ (addUsageFree uses UsedPred) ts
	
	_ -> panic stage $ "slurpUsageOther: no match"
