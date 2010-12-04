{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Constraint.Simplify.Collect
	( Table (..)
	, collect)
where
import DDC.Constraint.Simplify.Usage
import DDC.Constraint.Exp
import DDC.Type
import DDC.Var
import Control.Monad
import Data.Hashable
import Data.MaybeUtil
import Data.HashTable			(HashTable)
import qualified Data.HashTable		as Hash


-- Table ------------------------------------------------------------------------------------------
-- | A table of bindings that can be safely inlined.
data Table
	= Table 
	{ -- | Equality contraints to inline.
	  tableEq	:: HashTable Var Type
	
	  -- | More-than constraints to inline.
	, tableMore	:: HashTable Var Type

	  -- | Table of constraint vars that we cannot inline because the left is a wanted
	  --   variable. We keep this information separate, instead of folding it into the 
	  --   above tables, so we can pass those directly to the type substitutor later.
	, tableBlocked	:: HashTable Var () }


-- | Provided a constraint variable is not in the noSub table, then remember that 
--   we want to inline it, otherwise drop it on the floor.
insertConstraint
	:: Table
	-> (Table -> HashTable Var Type)
	-> Var -> Type
	-> IO ()

{-# INLINE insertConstraint #-}
insertConstraint table proj var t
 = do	blocked	<- liftM isJust
		$ Hash.lookup (tableBlocked table) var
	
	if blocked 
	 then return ()
	 else do
		Hash.insert (tableBlocked table) var ()
		Hash.insert (proj table)         var t


insertEq :: Table -> Var -> Type -> IO ()
{-# INLINE insertEq #-}
insertEq   table var t = insertConstraint table tableEq   var t


-- insertMore table var t = insertConstraint table tableMore var t


-- | Remember that we cannot inline this variable,
--   and kick out any constraints we have already collected for it.
blockConstraint :: Table -> Var -> IO ()
{-# INLINE blockConstraint #-}
blockConstraint table var
 = do	Hash.insert (tableBlocked table) var ()
	Hash.delete (tableEq   table) var
	Hash.delete (tableMore table) var


-- Collect ----------------------------------------------------------------------------------------
collect :: UseMap
	-> CTree
	-> IO Table

collect usage cc
 = do	-- create the initial tabe.
	table	<- liftM3 Table 
			(Hash.new (==) hash)
			(Hash.new (==) hash)
			(Hash.new (==) hash)

	-- collect inlinable constraints from the tree.
	collectTree usage table cc

	return table
	

-- | Collect up a table of bindings that can be safely inlined.
collectTree 
	:: UseMap	-- ^ Map of how constrained variables are used.
	-> Table	-- ^ Table of inlinable constraints to add to.
	-> CTree	-- ^ Tree of constraints we're walking over.
	-> IO ()

collectTree uses table cc
 = let	-- Check whether some TVar is wanted by the Desugar->Core transform.
	-- If it's wanted then we can't inline it, otherwise we'll lose the name.
	{-# INLINE isWanted #-}
	isWanted t	= usedIsWanted uses t

   in case cc of
	CBranch{}
	 -> mapM_ (collectTree uses table) $ branchSub cc
	
	-- inline  v1 = v2 renames from the left
	CEq _   t1@(TVar _ (UVar v)) t2@TVar{}
	 -> do	wanted	<- isWanted t1
		when (not wanted)
		 $ insertEq table v t2

{-	-- inline  v1 = v2 renames from the left.
	CEq _   t1@(TVar _ (UVar v)) t2@TVar{}
	 | doNotWant t1
	 -> insertEq table v t2
-}
{-	CMore _ t1@TVar{} t2
	 | [(UsedMore OnLeft, 1), (UsedMore OnRight, 1)] <- lookupUsage t1 usage
	 -> singleEq t1 t2
-}
	CInst _ v _
	  -> blockConstraint table v
	
	_ -> return ()
