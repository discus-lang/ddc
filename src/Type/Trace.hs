
module Type.Trace 
	( traceType 
	, traceCidsDown
	, traceFetterCidsUp)
where

import qualified Data.Set	as Set
import Data.Set			(Set)

import Util
import Type.Exp
import Type.Base
import Type.State
import Type.Class
import Type.Plate.Collect
import Type.Plate.Trans


-----
-- traceType
--	Extract a type from the graph by tracing down from this cid
--
traceType :: ClassId -> SquidM Type
traceType cid
 = do 	
 	-- See which classes are reachable by tracing down from this one.
 	cidsDown	<- traceCidsDown cid

	-- See what fetter classes are reachable by tracing back up from these.
	cidsFetterUp	<- traceFetterCidsUp cidsDown

	let cidsReachable	= Set.union cidsDown cidsFetterUp

	t	<- loadType cid cidsReachable
	return t


-- | Extract type subgraph.
loadType 
	:: ClassId 	-- ^ root node of the type
	-> Set ClassId 	-- ^ all the cids reachable from the root
	-> SquidM Type

loadType cid cidsReachable
 = do
	-- Build a new let for each of the classes
	fsLet		<- liftM catMaybes
			$  mapM	loadTypeNode
			$  Set.toList cidsReachable

	k		<- kindOfCid cid
	let tTrace	= TFetters fsLet (TClass k cid)
	return tTrace


-- | Make a new fetter representing this node in the type graph.
loadTypeNode
	:: ClassId
	-> SquidM (Maybe Fetter)

loadTypeNode cid
 = do	Just c		<- lookupClass cid
	loadTypeNode2 cid c

loadTypeNode2 cid c
	| ClassFetter{}	<- c
	= do	t'	<- refreshCids $ classFetter c
		return	$ Just t'

	| TBot k	<- classType c
	=	return	$ Nothing

	| otherwise
	= do	t'	<- refreshCids $ classType c
		k	<- kindOfCid cid
		return $ Just (FLet (TClass k cid) t')

refreshCids xx
 	= transZM 
		transTableId 
 		{ transCid = \cid -> do
				cid'	<- sinkClassId cid
				return	$ cid' }
		xx 

-----
-- traceCidsDown 
--	Trace out the cids reachable by tracing down from this cid.
--
traceCidsDown :: ClassId -> SquidM (Set ClassId)
traceCidsDown cid
 = traceCidsDowns (Set.singleton cid) Set.empty


traceCidsDowns :: Set ClassId -> Set ClassId -> SquidM (Set ClassId)
traceCidsDowns toVisit visited
 = case takeHead $ Set.toList toVisit of
 	Nothing	-> return visited
	
	Just cid
	 -> do	Just c		<- lookupClass cid
		let visited'	=  Set.insert cid visited

		moreR		<- refreshCids $ collectClassIds $ classType c
		let more	=  Set.fromList moreR

		let toVisit'	= (toVisit `Set.union` more) `Set.difference` visited'
		traceCidsDowns toVisit' visited'


-----
-- traceFetterCidsUp
--	Find all the fetter classes which are reachable by tracing up from this
--	set of cids.
--
traceFetterCidsUp :: Set ClassId -> SquidM (Set ClassId)
traceFetterCidsUp roots
	= traceFetterCidsUp1 Set.empty Set.empty roots

traceFetterCidsUp1 visited acc work
	| Set.null work
	= return acc

traceFetterCidsUp1 visited acc work
	| otherwise
	= do	let (cid : workTail) = Set.toList work
		Just c	<- lookupClass cid
		traceFetterCidsUp2 visited acc work cid c workTail

traceFetterCidsUp2 visited acc work cid c workTail

	-- found one
	| c =@= ClassFetter{}
	= do	traceFetterCidsUp1
			(Set.insert cid visited)
			(Set.insert cid acc)
			(Set.delete cid work)

	-- class has no backrefs, but isn't a fetter - skip.
	| Set.null $ classBackRef c
	= 	traceFetterCidsUp1
			(Set.insert cid visited)
			acc
			(Set.delete cid work)

	-- class has some backrefs
	| otherwise
	= do	-- no point visiting nodes we've already seen
		let back	= Set.difference (classBackRef c) visited

		traceFetterCidsUp1
			(Set.insert cid visited)
			acc
			(Set.union (Set.delete cid work) back)
