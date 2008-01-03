
module Core.Clean
	(cleanTree)
	
where

import Core.Exp
import Core.Util
import Core.Plate.Walk		
import Shared.VarPrim
import Shared.Error

import qualified Data.Map 	as Map
import Data.Map			(Map)
import Control.Monad.State

import Util

stage	= "Core.Clean"

-- | Clean out empty effect and closure variables from this tree
--	Also substitute simple v1 = v2 bindings
--
--	Run the main cleaner twice to clean out vars where the only thing
--	holding them live is trivial (v = v) bindings.
--
cleanTree :: Tree -> Tree
cleanTree tt	= cleanTree' $ cleanTree' tt

cleanTree' tt	= evalState (walkTreeM table tt) ()

table	
 = walkTableId
 	{ transT	= cleanT 
	, transX	= cleanX }


-- | Clean this expression
type CleanM	= State ()


cleanT :: WalkTable CleanM -> Type -> CleanM Type
cleanT table tt
	-- If a type var is out of scope then it is unconstrained.
	--	Default it to () to make the lint checker happy.
	--
	--	TODO: If there are class constraints on this var then this is actually
	--	      an ambiguous variable. In this case rewriting it to () will cause a 
	--	      core type error, which we'll live with for now.
	| TVar KData v	<- tt
	, Nothing	<- Map.lookup v (boundK table)
	= return $ TData primUnit []

	-- If a closure or effect var has not been bound then we can erase it
	| TVar k v	<- tt
	, elem k [KEffect, KClosure]
	, Nothing	<- Map.lookup v (boundT table)
	, Nothing	<- Map.lookup v (boundK table)
	= return $ TBot k


	-- Effects on unbound regions can be masked.
	| TEffect vE [TVar KRegion vR]	<- tt
	, elem vE [primRead, primWrite]
	, Nothing	<- Map.lookup vR (boundK table)
	= return $ TBot KEffect

	-- Repack effect and closure sums on the way up to crush out TPure/Empty added above
	| TSum k ts	<- tt
	= return $ makeTSum k $ flattenTSum tt
		
	| otherwise
	= return tt


cleanX :: WalkTable CleanM -> Exp -> CleanM Exp
cleanX table xx
 = case xx of
 	XTet vts x	
	  -> return $ XTet (catMaybes $ map eatIdBind vts) x
	_ -> return xx


eatIdBind :: (Var, Type) -> Maybe (Var, Type)
eatIdBind (v1, t)
 = case t of
	TVar k v2 	
	 | v1 == v2	-> Nothing
	 
	_ 		-> Just (v1, t)
