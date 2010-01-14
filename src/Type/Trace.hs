
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


-- | Extract subgraph from the type graph.
--	Along the way, we reuse TFree fetters where ever possible.
--	Instead of returning a type like:
--
--	  *1 :- ... 
--	     , $2 :> x : *4
--	     , $3 :> x : *4
--
--	It's better to use
--
--	  *1 :- ... 
--	     , $2 :> x : *4
--	     , $3 :> $2
--
--     This saves work in our naive type packer, as it doesn't have
--     to build the type of *4 twice. If we had a better (and more complicated)
--     packer we wouldn't have to do this.
--
loadType 
	:: ClassId 	-- ^ root node of the type
	-> Set ClassId 	-- ^ all the cids reachable from the root
	-> SquidM Type

loadType cid cidsReachable
 = do
	-- Build a new constraint for each of the reachable classes
	(_, fss)	
		<- mapAccumLM loadTypeNode [] 
		$  Set.toList cidsReachable

	let fs	= concat fss
	k	<- kindOfCid cid
	return	$ TFetters (TClass k cid) fs


-- | Make new fetters representing this node in the type graph.
loadTypeNode
	:: [Fetter]		-- TFree fetters
	-> ClassId
	-> SquidM 
		( [Fetter]
		, [Fetter] )

loadTypeNode fsTFree cid@(ClassId ix)
 = do	Just c	<- lookupClass cid
	loadTypeNode2 fsTFree cid c

loadTypeNode2 fsTFree cid c

	-- when mptc's are crushed out they are replaced by ClassNils.
	-- we could perhaps differentiate this case and raw, never-allocated classes...
	| ClassNil		<- c
	= return (fsTFree, [])

	| ClassForward cid'	<- c
	= loadTypeNode fsTFree cid'

	| ClassFetter { classFetter = f } <- c
	= do	t'	<- refreshCids f
		return	(fsTFree, [f])

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
		-- make sure all the cids are canonical
		tRefreshed	<- refreshCids tNode
		fsSingle	<- refreshCids $ map fst $ classFetterSources c

		-- chop of fetters attached directly to the type in the node
		let (tBody, fsLocal) 	
			= case tRefreshed of
				TFetters t fs	-> (t, fs)
				t		-> (t, [])

		-- multi parameter constraints are stored in nodes referenced by classFettersMulti
		fsMulti		<- liftM (concat . (map snd))
				$  mapM (loadTypeNode [])
				$  Set.toList 
				$  classFettersMulti c

		var		<- makeClassName cid
		quantVars	<- gets stateQuantifiedVars
	
		let fs	= nub (fsLocal ++ fsSingle ++ fsMulti)

		loadTypeNode3 fsTFree cid k tBody fs

loadTypeNode3 fsTFree cid k tBody fs
	-- We don't need to return TBot constraints.
	--	If a variable has no constraint its already :> Bot.
	| TBot k	<- tBody
	= return (fsTFree, fs)
			
	-- Trim closures as early as possible to avoid wasing time in later stages.
	| resultKind k == kClosure
	= case trimClosureC Set.empty Set.empty tBody of
		TFree _ (TBot _)	
		 -> return (fsTFree, fs)

		clo@TFree{}
		 | Just (FMore t1Cache _)
			<- find (\f -> case f of
					FMore _ t2 -> t2 == clo
					_	   -> False )
				fsTFree
		 -> return
			( fsTFree
			, FMore (TClass k cid) t1Cache : fs )
			
		tX_trimmed
		 -> let fHere	= FMore (TClass k cid) tX_trimmed
		    in  return 
			  ( fHere : fsTFree
			  , fHere : fs )
		
	-- Use equality for type constraints.
	| resultKind k == kValue
	= return ( fsTFree
		 , FWhere (TClass k cid) tBody : fs )

	-- Use :> for effect and closure constraints.
	| otherwise
	= return ( fsTFree
		 , FMore  (TClass k cid) tBody : fs )


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

