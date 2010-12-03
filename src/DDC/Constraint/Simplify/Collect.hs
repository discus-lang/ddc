{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# OPTIONS -Wnot #-}
module DDC.Constraint.Simplify.Collect
	( Table (..)
	, collect)
where
import DDC.Constraint.Simplify.Usage
import DDC.Constraint.Exp
import DDC.Type
import DDC.Var
import DDC.Main.Pretty
import DDC.Main.Error
import Control.Monad
import Data.Hashable
import Data.Monoid
import Data.Set				(Set)
import Data.Map				(Map)
import Data.HashTable			(HashTable)
import qualified Data.HashTable		as Hash
import qualified Data.Foldable		as Seq
import qualified Data.Set		as Set
import qualified Data.Map		as Map
import qualified Debug.Trace

stage		= "DDC.Constraint.Collect"
debug		= True
trace ss x	= if debug then Debug.Trace.trace (pprStrPlain ss) x else x


-- Table ------------------------------------------------------------------------------------------
-- | A table of bindings that can be safely inlined.
--   If the value is Nothing this means we cannot inine the constraint.
data Table
	= Table 
	{ tableEq	:: HashTable Var (Maybe Type)
	, tableMore	:: HashTable Var (Maybe Type) }


-- | Insert an eq constraint into the table.
insertConstraint
	:: HashTable Var (Maybe Type)
	-> Var -> Type
	-> IO ()

insertConstraint table var t
 = do	mval	<- Hash.lookup table var
	case mval of 
	 -- insert new constraint.
	 Nothing	-> Hash.insert table var (Just t)
	
	 -- cannot inine this, drop it on the floor.
	 Just Nothing	-> return ()
	
	 -- we can't hold multiple constraints in the table.
	 Just (Just _)	-> Hash.insert table var Nothing
	
insertEq   table var t = insertConstraint (tableEq   table) var t
insertMore table var t = insertConstraint (tableMore table) var t


-- | Kick out any existing constraint and set the entry to Nothing so more more are added.
blockConstraint
	:: HashTable Var (Maybe Type)
	-> Var
	-> IO ()

blockConstraint table var
 = do	Hash.insert table var Nothing

blockCrs  table	v
 = do	blockConstraint (tableEq table)   v
	blockConstraint (tableMore table) v


-- Collect ----------------------------------------------------------------------------------------
collect :: UseMap
	-> CTree
	-> IO Table

collect usage cc
 = do	eqs		<- Hash.new (==) hash
	mores		<- Hash.new (==) hash
	let table	= Table eqs mores
	collectTree usage table cc
	return table
	

-- | Collect up a table of bindings that can be safely inlined.
collectTree 
	:: UseMap
	-> Table
	-> CTree
	-> IO ()

collectTree usage table cc
 = let doNotWant t 
		= not $ elem UsedWanted $ map fst
		$ lookupUsage t usage

   in case cc of
	CBranch{}
	 -> mapM_ (collectTree usage table) $ branchSub cc
	
	-- inline  v1 = v2 renames from the right.
	CEq _   t1@(TVar _ (UVar v)) t2@TVar{}
	 | doNotWant t2
	 -> insertEq table v t2

	-- inline  v1 = v2 renames from the left.
	CEq _   t1@(TVar _ (UVar v)) t2@TVar{}
	 | doNotWant t1
	 -> insertEq table v t2

{-	CMore _ t1@TVar{} t2
	 | [(UsedMore OnLeft, 1), (UsedMore OnRight, 1)] <- lookupUsage t1 usage
	 -> singleEq t1 t2
-}
	CInst _ v _
	  -> blockCrs table v
	
	_ -> return ()
