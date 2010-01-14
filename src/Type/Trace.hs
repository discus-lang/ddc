
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
	t	<- loadTypeNodes cid (Set.toList cidsDown) [] Map.empty Map.empty [] 
	return t


-- | Make new fetters representing this node in the type graph.
loadTypeNodes
	:: ClassId		-- cid of body
	-> [ClassId]		-- cids of classes reachable from body
	-> [Fetter]		-- TFetters found so far
	-> Map Type Type	-- acc equality fetters
	-> Map Type Type	-- acc more than fetters
	-> [Fetter]		-- acc other fetters
	-> SquidM Type
	
loadTypeNodes cidBody [] fsTFree fsAccEq fsAccMore fsAccOther
 = do	k	<- kindOfCid cidBody
	return	$ TConstrain 
			(TClass k cidBody) 
			(Constraints fsAccEq fsAccMore fsAccOther)
	
loadTypeNodes cidBody (cidReach1 : cidsReach) fsTFree fsAccEq fsAccMore fsAccOther
 = do	Just clsReach1	<- lookupClass cidReach1
	loadTypeNodes2 
		cidBody (cidReach1 : cidsReach) 
		fsTFree fsAccEq fsAccMore fsAccOther clsReach1

loadTypeNodes2 cidBody (cidReach1 : cidsReach) fsTFree fsAccEq fsAccMore fsAccOther clsReach1

	-- when mptc's are crushed out they are replaced by ClassNils.
	-- we could perhaps differentiate this case and raw, never-allocated classes...
	| ClassNil		<- clsReach1
	= loadTypeNodes cidBody cidsReach 
		fsTFree fsAccEq fsAccMore fsAccOther

	| ClassForward cid'	<- clsReach1
	= loadTypeNodes cidBody (cid' : cidsReach) 
		fsTFree fsAccEq fsAccMore fsAccOther

	-- add this fetter to the list
	| ClassFetter { classFetter = f } <- clsReach1
	= do	f'	<- refreshCids f
		loadTypeNodes cidBody cidsReach 
			fsTFree fsAccEq fsAccMore (f' : fsAccOther)

	-- If the class type is Nothing then it hasn't been unified yet..
	| Class { classType = Nothing }	  <- clsReach1
	= panic stage
		$ "loadTypeNode2: can't trace which hasn't been unified yet\n"
		% "    cid        = "  % cidReach1				% "\n"
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
			= case toConstrainFormT tRefreshed of
				TConstrain t crs	-> (t, crs)
				t			-> (t, Constraints Map.empty Map.empty [])

		var		<- makeClassName cidReach1
		quantVars	<- gets stateQuantifiedVars
	
		loadTypeNodes3 
			cidBody 
			(cidReach1 : (Set.toList $ classFettersMulti clsReach1) ++ cidsReach) 
			fsTFree 
			(Map.union fsAccEq   (crsEq   fsLocal))
			(Map.union fsAccMore (crsMore fsLocal))
			(fsSingle ++ crsOther fsLocal ++ fsAccOther)
			k tBody

loadTypeNodes3 cidBody (cidReach1 : cidsReach) 
	fsTFree fsAccEq fsAccMore fsAccOther k tBody

	-- We don't need to return TBot constraints.
	--	If a variable has no constraint its already :> Bot.
	| TBot k	<- tBody
	= loadTypeNodes cidBody cidsReach 
		fsTFree fsAccEq fsAccMore fsAccOther
	
	-- Trim closures as early as possible to avoid wasing time in later stages.
	| resultKind k == kClosure
	= case trimClosureC Set.empty Set.empty (toFetterFormT tBody) of
		TFree _ (TBot _)	
		 -> loadTypeNodes cidBody cidsReach 
			fsTFree fsAccEq fsAccMore fsAccOther
		
		clo@TFree{}
		 | Just (FMore t1Cache _)
			<- find (\f -> case f of
					FMore _ t2 -> t2 == clo
					_	   -> False )
				fsTFree

		 -> loadTypeNodes cidBody cidsReach 
				fsTFree 
				fsAccEq
				(Map.insert (TClass k cidReach1) t1Cache fsAccMore)
				fsAccOther
					
		tX_trimmed
		 -> let fHere	= FMore (TClass k cidReach1) tX_trimmed
		    in  loadTypeNodes cidBody cidsReach 
				(fHere : fsTFree)
				fsAccEq
				(Map.insert (TClass k cidReach1) tX_trimmed fsAccMore)
				fsAccOther
				
		
	-- Use equality for type constraints.
	| resultKind k == kValue
	= loadTypeNodes cidBody cidsReach 
			fsTFree 
			(Map.insert (TClass k cidReach1) tBody fsAccEq) 
			fsAccMore 
			fsAccOther
			

	-- Use :> for effect and closure constraints.
	| otherwise
	= loadTypeNodes cidBody cidsReach 
			fsTFree 
			fsAccEq
			(Map.insert (TClass k cidReach1) tBody fsAccMore)
			fsAccOther


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

