
module Type.Trace 
	( traceType 
	, traceCidsDown)
where
import Type.Exp
import Type.Base
import Type.Util
import Type.State
import Type.Class
import Type.Plate.Collect
import Type.Plate.Trans

import Shared.Pretty
import Shared.Error
import Debug.Trace

import Util
import qualified Data.Set	as Set
import qualified Data.Map	as Map
import qualified Debug.Trace	as Trace

stage	= "Type.Trace"

-- | Extract a type node from the graph by tracing down from this cid
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
	fsLet	<- liftM concat
		$  mapM	loadTypeNode
		$  Set.toList cidsReachable

	k	<- kindOfCid cid
	return	$ TFetters (TClass k cid) fsLet


-- | Make new fetters representing this node in the type graph.
loadTypeNode
	:: ClassId
	-> SquidM [Fetter]

loadTypeNode cid@(ClassId ix)
 = do	Just c	<- lookupClass cid
	loadTypeNode2 cid c

loadTypeNode2 cid c

	-- when mptc's are crushed out they are replaced by ClassNils.
	-- we could perhaps differentiate this case and raw, never-allocated classes...
	| ClassNil		<- c
	= return []

	| ClassForward cid'	<- c
	= loadTypeNode cid'

	| ClassFetter { classFetter = f } <- c
	= do	t'	<- refreshCids f
		return	[t']

	-- If the class type is Nothing then it hasn't been unified yet..
	| Class { classType = Nothing }	  <- c
	= panic stage
		$ "loadTypeNode2: can't trace which hasn't been unified yet\n"
		% "    cid        = "  % cid		% "\n"
		% "    queue      = "  % classQueue c	% "\n"
		% "    typeSources:\n" % "\n" %!% classTypeSources c % "\n"

	-- a regular type node
	| Class { classType = Just tNode
		, classKind = k } 	<- c
	= do
		-- make sure all the cids are canconical
		tRefreshed	<- refreshCids tNode
		fsSingle	<- refreshCids $ map fst $ classFetterSources c

		-- chop of fetters attached directly to the type in the node
		let (tBody, fsLocal) 	
			= case tRefreshed of
				TFetters t fs	-> (t, fs)
				t		-> (t, [])

		-- multi parameter constraints are stored in nodes referenced by classFettersMulti
		fsMulti		<- liftM concat 
					$ mapM loadTypeNode
					$ Set.toList 
					$ classFettersMulti c

		var		<- makeClassName cid
		quantVars	<- gets stateQuantifiedVars
	
		let fs	= nub (fsLocal ++ fsSingle ++ fsMulti)

		let (result :: SquidM [Fetter])

			-- We don't need to return TBot constraints.
			--	If a variable has no constraint its already :> Bot.
			| TBot k	<- tBody
			= return fs
	
			-- Trim closures as early as possible to avoid wasing time in later stages.
			| resultKind k == kClosure
			= case trimClosureC Set.empty Set.empty tBody of
				TFree _ (TBot _)	-> return fs
				tX_trimmed		-> return $ FMore (TClass k cid) tX_trimmed : fs
	
			-- Use equality for type constraints.
			| resultKind k == kValue
			= return $ FWhere (TClass k cid) tBody 
				 : fs
			
			-- Use :> for effect and closure constraints.
			| otherwise
			= return $ FMore  (TClass k cid) tBody 
				 : fs
		
		result
	

refreshCids xx
 	= transZM 
		transTableId 
 		{ transCid = \cid -> do
				cid'	<- sinkClassId cid
				return	$ cid' }
		xx 


-- | Trace out the classes reachable from this one.
traceCidsDown :: ClassId -> SquidM (Set ClassId)
traceCidsDown cid
 = traceCidsDowns (Set.singleton cid) Set.empty


-- | Trace out the classes reachable from this set.
traceCidsDowns 
	:: Set ClassId 			-- ^ root set
	-> Set ClassId 			-- ^ classes already visited
	-> SquidM
	 	(Set ClassId)		-- ^ classes reachable from the root set

traceCidsDowns toVisit visited
 = case takeHead $ Set.toList toVisit of
 	Nothing	-> return visited
	
	Just cid
	 -> do	Just c		<- lookupClass cid
		let visited'	=  Set.insert cid visited

		moreR		<- refreshCids $ classChildren c
		let more	=  Set.fromList moreR

		let toVisit'	= (toVisit `Set.union` more) 
					`Set.difference` visited'

		traceCidsDowns toVisit' visited'
