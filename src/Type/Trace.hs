
module Type.Trace 
	( traceType 
	, traceType_shape
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
	t	<- loadType True cid cidsDown
	return t

-- Trace the shape of this type, no fetters
traceType_shape :: ClassId -> SquidM Type
traceType_shape cid
 = do	-- See which classes are reachable by tracing down from this one.
 	cidsDown	<- {-# SCC "trace/Down" #-} traceCidsDown cid

	-- Load in the nodes for this subgraph.
	t	<- loadType False cid cidsDown
	return t

-- LoadType ----------------------------------------------------------------------------------------
-- | Extract type subgraph.
loadType 
	:: Bool		-- ^ whether to include fetters
	-> ClassId 	-- ^ root node of the type
	-> Set ClassId 	-- ^ all the cids reachable from the root
	-> SquidM Type

loadType incFs cid cidsReachable
 = do
	-- Build a new let for each of the classes
	fsLet		<- liftM concat
			$  mapM	(loadTypeNode incFs)
			$  Set.toList cidsReachable

	k		<- kindOfCid cid
	let tTrace	= TFetters (TClass k cid) fsLet
	return tTrace


-- | Make new fetters representing this node in the type graph.
loadTypeNode
	:: Bool		-- ^ whether to include fetters
	-> ClassId
	-> SquidM [Fetter]

loadTypeNode incFs cid@(ClassId ix)
 = do	Just c		<- lookupClass cid
	loadTypeNode2 incFs cid c

loadTypeNode2 incFs cid c

	-- when mptc's are crushed out they are replaced by ClassNils.
	-- we could perhaps differentiate this case and raw, never-allocated classes...
	| ClassNil				<- c
	= return []

	| ClassForward cid'			<- c
	= loadTypeNode incFs cid'

	| ClassFetter { classFetter = f } 	<- c
	= do	t'	<- refreshCids f
		return	[t']

	-- If the class type is Nothing then it hasn't been unified yet..
	| Class { classType = Nothing}		<- c
	= panic stage
		$ "loadTypeNode2: can't trace which hasn't been unified yet\n"
		% "    cid        = " % cid		% "\n"
		% "    queue      = " % classQueue c	% "\n"
		% "    typeSources:\n" % "\n" %!% classTypeSources c % "\n"

	-- a regular type node
	| Class { classType = Just tNode
		, classKind = k } 	<- c
	= do
		-- make sure all the cids are canconical
		tRefreshed	<- refreshCids tNode
		tFs		<- refreshCids $ map fst $ classFetterSources c

		-- chop of fetters attached directly to the type in the node
		let (tX, fsLocal) 	
			= case tRefreshed of
				TFetters t fs	-> (t, fs)
				t		-> (t, [])

		-- single parameter constraints are stored directly in this node
		let fsSingle	= if incFs
					then tFs
					else []

		-- multi parameter constraints are stored in nodes referenced by classFettersMulti
		fsMulti		<- if incFs
					then liftM concat 
						$ mapM (loadTypeNode incFs)
						$ Set.toList 
						$ classFettersMulti c
					else return []

		var		<- makeClassName cid
		quantVars	<- gets stateQuantifiedVars
	
		let fs	= nub (fsLocal ++ fsSingle ++ fsMulti)

		let (result :: SquidM [Fetter])
			-- don't bother showing bottom constraints
			--	If a constraint for a class is missing it is assumed to be bottom.
			| TBot k	<- tX
			= return fs
	
			| resultKind k == kClosure
			= case trimClosureC Set.empty Set.empty tX of
				TFree _ (TBot _)	-> return fs
				tX_trimmed		-> return $ FMore (TClass k cid) tX_trimmed : fs
	
			| resultKind k == kValue
			= 	return $ FWhere (TClass k cid) tX : fs
			
			| otherwise
			= 	return $ FMore  (TClass k cid) tX : fs
		
		result
	

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
