
module Core.Clean
	(cleanTree)
	
where

import Core.Exp
import Core.Util
import Core.Plate.Walk		
import Shared.VarPrim

import qualified Data.Map 	as Map
import Data.Map			(Map)
import Control.Monad.State


-- | Clean out empty effect and closure variables from this tree
--	Also substitute simple v1 = v2 bindings
--
cleanTree :: Tree -> Tree
cleanTree tt	
	= evalState (walkZM table tt) ()

table	
 = walkTableId
 	{ transT	= cleanT }


-- | Clean this expression
type CleanM	= State ()



cleanT :: WalkTable CleanM -> Type -> CleanM Type
cleanT table tt
	-- If a closure or effect var has not been bound then we can erase it
	| TVar k v	<- tt
	, elem k [KEffect, KClosure]
	, Nothing	<- Map.lookup v (boundT table)
	= return $ TBot k

	-- Effects on unbound regions can be masked.
	| TEffect vE [TVar KRegion vR]	<- tt
	, elem vE [primRead, primWrite]
	, Nothing	<- Map.lookup vR (boundT table)
	= return $ TBot KEffect

	-- Repack effect and closure sums on the way up to crush out TPure/Empty added above
	| TSum k ts	<- tt
	= return $ makeTSum k $ flattenTSum tt
		
	| otherwise
	= return tt

