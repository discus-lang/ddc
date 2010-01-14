
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


-- | Extract a type node from the graph by tracing down from this cid.
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
traceType :: ClassId -> SquidM Type
traceType cid
 = do 	
 	-- See which classes are reachable by tracing down from this one.
 	cidsDown	<- {-# SCC "trace/Down" #-} traceCidsDown cid

	-- Load in the nodes for this subgraph.
	t	<- loadTypeNodes cid (Set.toList cidsDown) [] [] 
	return t


-- | Make new fetters representing this node in the type graph.
loadTypeNodes
	:: ClassId		-- cid of body
	-> [ClassId]		-- cids of classes reachable from body
	-> [Fetter]		-- TFetters found so far
	-> [Fetter]		-- fetter accumulator to add to body
	-> SquidM Type
	
loadTypeNodes cidBody [] fsTFree fsAcc
 = do	k	<- kindOfCid cidBody
	return	$ TFetters (TClass k cidBody) fsAcc
	
loadTypeNodes cidBody (cidReach1 : cidsReach) fsTFree fsAcc
 = do	Just clsReach1	<- lookupClass cidReach1
	loadTypeNodes2 cidBody (cidReach1 : cidsReach) fsTFree fsAcc clsReach1

loadTypeNodes2 cidBody (cidReach1 : cidsReach) fsTFree fsAcc clsReach1

	-- when mptc's are crushed out they are replaced by ClassNils.
	-- we could perhaps differentiate this case and raw, never-allocated classes...
	| ClassNil		<- clsReach1
	= loadTypeNodes cidBody cidsReach fsTFree fsAcc

	| ClassForward cid'	<- clsReach1
	= loadTypeNodes cidBody (cid' : cidsReach) fsTFree fsAcc

	-- add this fetter to the list
	| ClassFetter { classFetter = f } <- clsReach1
	= do	f'	<- refreshCids f
		loadTypeNodes cidBody cidsReach fsTFree (f' : fsAcc)

	-- If the class type is Nothing then it hasn't been unified yet..
	| Class { classType = Nothing }	  <- clsReach1
	= panic stage
		$ "loadTypeNode2: can't trace which hasn't been unified yet\n"
		% "    cid        = "  % cidReach1					% "\n"
		% "    queue      = "  % classQueue clsReach1			% "\n"
		% "    typeSources:\n" % "\n" %!% classTypeSources clsReach1 	% "\n"

	-- a regular type node
	| Class { classType = Just tNode
		, classKind = k } 	<- clsReach1
	= do
		-- make sure all the cids are canonical
		tRefreshed	<- refreshCids tNode
		fsSingle	<- refreshCids $ map fst $ classFetterSources clsReach1

		-- chop of fetters attached directly to the type in the node
		let (tBody, fsLocal) 	
			= case tRefreshed of
				TFetters t fs	-> (t, fs)
				t		-> (t, [])

		var		<- makeClassName cidReach1
		quantVars	<- gets stateQuantifiedVars
	
		let fsHere	= nub (fsLocal ++ fsSingle)

		loadTypeNodes3 
			cidBody 
			(cidReach1 : (Set.toList $ classFettersMulti clsReach1) ++ cidsReach) 
			fsTFree 
			(fsHere ++ fsAcc) 
			k tBody

loadTypeNodes3 cidBody (cidReach1 : cidsReach) fsTFree fsAcc k tBody
	-- We don't need to return TBot constraints.
	--	If a variable has no constraint its already :> Bot.
	| TBot k	<- tBody
	= loadTypeNodes cidBody cidsReach fsTFree fsAcc
	
	-- Trim closures as early as possible to avoid wasing time in later stages.
	| resultKind k == kClosure
	= case trimClosureC Set.empty Set.empty tBody of
		TFree _ (TBot _)	
		 -> loadTypeNodes cidBody cidsReach fsTFree fsAcc
		
		clo@TFree{}
		 | Just (FMore t1Cache _)
			<- find (\f -> case f of
					FMore _ t2 -> t2 == clo
					_	   -> False )
				fsTFree

		 -> let fHere	=  FMore (TClass k cidReach1) t1Cache
		    in  loadTypeNodes cidBody cidsReach 
				fsTFree 
				(fHere : fsAcc)
					
		tX_trimmed
		 -> let fHere	= FMore (TClass k cidReach1) tX_trimmed
		    in  loadTypeNodes cidBody cidsReach 
				(fHere : fsTFree)
				(fHere : fsAcc)
		
	-- Use equality for type constraints.
	| resultKind k == kValue
	= let fHere	= FWhere (TClass k cidReach1) tBody
	  in  loadTypeNodes cidBody cidsReach fsTFree (fHere : fsAcc)

	-- Use :> for effect and closure constraints.
	| otherwise
	= let fHere	= FMore  (TClass k cidReach1) tBody
	  in  loadTypeNodes cidBody cidsReach fsTFree (fHere : fsAcc)


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

