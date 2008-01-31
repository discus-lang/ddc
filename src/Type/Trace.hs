
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
import Type.Dump
import Type.State
import Type.Class
import Type.Plate.Collect
import Type.Plate.Trans

import Shared.Error
import Debug.Trace

stage	= "Type.Trace"

-----
-- traceType
--	Extract a type from the graph by tracing down from this cid
--
traceType :: ClassId -> SquidM Type
traceType cid
 = do 	
 	-- See which classes are reachable by tracing down from this one.
 	cidsDown	<- {-# SCC "trace/Down" #-} traceCidsDown cid

	-- Load in the nodes for this subgraph.
	t	<- loadType cid cidsDown
	return t


-- | Extract type subgraph.
loadType 
	:: ClassId 	-- ^ root node of the type
	-> Set ClassId 	-- ^ all the cids reachable from the root
	-> SquidM Type

loadType cid cidsReachable
 = do
	-- Build a new let for each of the classes
	fsLet		<- liftM concat
			$  mapM	loadTypeNode
			$  Set.toList cidsReachable

	k		<- kindOfCid cid
	let tTrace	= TFetters fsLet (TClass k cid)
	return tTrace


-- | Make new fetters representing this node in the type graph.
loadTypeNode
	:: ClassId
	-> SquidM [Fetter]

loadTypeNode cid@(ClassId ix)
 = do	Just c		<- lookupClass cid
	loadTypeNode2 cid c

loadTypeNode2 cid c

	-- when mptc's are crushed out they are replaced by ClassNils.
	-- we could perhaps differentiate this case and raw, never-allocated classes...
	| ClassNil				<- c
	= return []

	| ClassForward cid'			<- c
	= loadTypeNode cid'

	| ClassFetter { classFetter = f } 	<- c
	= do	t'	<- refreshCids f
		return	[t']

	-- If the class type is Nothing then it hasn't been unified yet..
	| Class { classType = Nothing}		<- c
	= panic stage
		$ "loadTypeNode2: can't trace which hasn't been unified yet\n"
		% "    cid   = " % cid		% "\n"
		% "    queue = " % classQueue c	% "\n"
		% "    nodes:\n" % "\n" %!% classNodes c % "\n"

	-- a regular type node
	| Class { classType = Just t}		<- c
	= do
		-- make sure all the cids are canconical
		(t': tfs)	<- refreshCids (t : classFetters c)

		-- single parameter constraints are stored directly in this node
		let fs		= map (\(TFetter f) -> f) tfs

		-- multi parameter constraints are stored in nodes referenced by classFettersMulti
		fsMulti		<- liftM concat 
				$ mapM loadTypeNode 
				$ Set.toList 
				$ classFettersMulti c

		-- If this node has additional fetters where the LHS are all cids
		--	then we can strip them here because they're cids they'll
		--	already be returned as their separate nodes
		let tX	= case t' of
				TFetters fs t2	
				 | and	$ map (\f -> case f of
				 		FLet  (TClass _ _) _	-> True
						FMore (TClass _ _) _	-> True
						_			-> False)
					$ fs
				 -> t2
				_	-> t'
				
		k	<- kindOfCid cid
	
		case t of
			-- don't bother showing bottom constraints
			--	If a constraint for a class is missing it is assumed to be bottom.
			TBot k	-> return $ fs ++ fsMulti

			_	-> return $ FLet (TClass k cid) tX : (fs ++ fsMulti)
	

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

		moreR		<- refreshCids $ classChildren c
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
	| ClassFetter{}	<- c
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


