{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Extract a subgraph from the main type graph.
--   It's called "trace" because we trace out all the nodes reachable from a given
--   class. This returns a single outer cid, and a list of constraints. 
--
--   @
--    Eg:  *1 :- ...
--            ,  *1 = Int32 %r1
--            ,  *2 = (->) *1 
--            ...
--            ,  $2 :> x : *4
--            ,  !3 :> {!5, !6}
--   @
--
--   Use Type.Pack to pack these constraints into a normal form type. We separate
--   tracing and packing into two stages because the traced type might contain
--   loops through the constraints, and it's easier to deal with these in a
--   separate pass.
--
--   Along the way, we reuse TFree fetters where ever possible.
--   Instead of returning a type like:
--
--   @
--        *1 :- ...
--           , $2 :> x : *4
--           , $3 :> x : *4
--   @
--
--	It's better to use:
--
--   @
--	  *1 :- ... 
--	     , $2 :> x : *4
--	     , $3 :> $2
--   @
--
--   This saves work in our naive type packer, as it doesn't have
--   to build the type of *4 twice. If we had a better (and more complicated)
--   packer we wouldn't have to do this.
--
module DDC.Solve.Trace 
	( traceType
	, traceTypeAsSquid
	, takeShallowTypeOfCid
	, takeShallowTypeOfCidAsSquid
	, getTypeOfNodeAsSquid)
where
import DDC.Solve.State.Base
import DDC.Solve.State.Graph
import DDC.Solve.State.Node
import DDC.Solve.State.Class
import DDC.Solve.State.Squid
import DDC.Solve.State.SinkIO
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Type
import Data.Array.IO
import Data.Foldable
import Data.List
import Type.Dump		()
import Data.Sequence		(Seq)
import Data.Set			(Set)
import Data.Map			(Map)
import qualified Data.Set	as Set
import qualified Data.Map	as Map
import qualified Data.Sequence	as Seq
import Control.Monad.State	hiding (mapM_)
import Prelude			hiding (mapM_)

stage		= "DDC.Solve.Trace"

-- Squid Monad Versions --------------------------------------------------------------------------
-- | Extract a type node from the graph by tracing down from this cid (in the Squid monad)
traceTypeAsSquid :: ClassId -> SquidM Type
traceTypeAsSquid cid
	= runTraceAsSquid (traceType cid)

takeShallowTypeOfCidAsSquid :: ClassId -> SquidM (Maybe Type)
takeShallowTypeOfCidAsSquid cid
	= runTraceAsSquid (takeShallowTypeOfCid cid)

getTypeOfNodeAsSquid :: Kind -> Node -> SquidM Type
getTypeOfNodeAsSquid kind node
	= runTraceAsSquid (getTypeOfNode kind node)


-- Trace ------------------------------------------------------------------------------------------
-- | Extract a type node from the graph by tracing down from this cid.
--   TODO: Store SPTCs in a map, instead of sequence. Just use the sequence for MPTCs. 
--         This would save having to nub them when we're constructing the final type.
--	
traceType :: ClassId -> TraceM Type
traceType cid'
 = do	cid		<- sinkCid cid'

	-- Load all the reachable constraints into the state
	traceFromCid cid
	
	-- Get constraints out of the state
	crsEq		<- gets stateAccEq
	crsMore		<- gets stateAccMore 
	crsOther	<- gets stateAccOther
	
	-- Trim and merge closures in the map of more constraints.
	let crsMore'	= simplifyCrsMore crsMore
	
	-- The final type
	Just t		<- takeShallowTypeOfCid cid
	let k		= kindOfType t
	let tt		= makeTConstrain 
				(TVar k $ UClass cid) 
				(Constraints crsEq crsMore' $ nub $ toList crsOther)
						
	return tt


-- | Recursively add type constraints starting from this classid to the trace state.
traceFromCid :: ClassId -> TraceM ()
traceFromCid cid
 = do	cidsVisited	<- gets stateCidsVisited
	if Set.member cid cidsVisited
	 then return ()
	 else traceFromCid' cid
	
traceFromCid' cid
 = do	
	-- Grab the corresponding class.
	cls	<- getClass cid

	-- Remember that we've already looked at this class.
	modify $ \s -> s { stateCidsVisited = Set.insert cid (stateCidsVisited s) }

	case cls of

	 -- uh oh, an unallocated part of the graph.
	 ClassUnallocated
	  -> 	panic stage $ "traceFromCid': class " % cid % " isn't allocated."

	 -- A forward reference.
	 ClassForward _ cid'
	  -> 	traceFromCid cid'

	 -- A regular class.
	 Class	{ classKind	= kind }
	  -> do	
		Just t	<- takeShallowTypeOfClass cls

		-- Split up the info from this type and add it to the state
		addType (TVar kind $ UClass cid) t
		
		-- Add the SPTCs directly in the class.
		let crsOther	= [FConstraint v [TVar kind $ UClass cid] 
					| v <- Map.keys $ classFetters cls]
		
		addCrsOther	$ Seq.fromList crsOther

		-- Decend into other classes reachable from this one.
		let cids	= Set.union (freeCids t) (classFettersMulti cls)
		mapM_ traceFromCid $ Set.toList cids

	 -- A deleted class.
	 ClassFetterDeleted _
	  -> return ()

	 -- A class holding a multi-param type constraint.
	 ClassFetter
		{ classFetter	= fetter }

	  -> do	-- Add the fetter to the state
		fetter'		<- sinkCidsInFetter fetter
		addCrsOther	$ Seq.singleton fetter'
		
		-- Decend into other classes reachable from this one.
		let cids	= freeCids fetter'
		mapM_ traceFromCid $ Set.toList cids



-- | Break up a type constraint into parts and add them to the trace state.
addType :: Type -> Type -> TraceM ()
addType t1 t2

	-- If the scheme in a TFree has an outer TConstrain we can split those constraints off.
	| Just (v, t21)	<- takeTFree t2
	= do	let (t21', crsEq', crsMore', crsOther')	
			= splitTConstrain t21
			
		let Just t2'	= makeTFree v t21'
			
		addCrsEq 	crsEq'
		addCrsMore 	(Map.union (Map.singleton t1 t2') crsMore')
		addCrsOther	crsOther'

	| otherwise
	= do	
		-- If the type we took from the class was a type scheme, then it may have an outer
		-- TConstrain node. We break these constraints off and return them as part of the
		-- traced type, to reduce possible duplication.
		let (t', crsEq', crsMore', crsOther')	
			= splitTConstrain t2
		
		-- Build the constraint for this class.
		--	If it's of value kind then use an eq constraint,
		--	otherwise it's a more constraint.
		let kind = kindOfType t1

		let crsEqHere	
			= if (not $ isTBot t') && resultKind kind == kValue
				then Map.singleton t1 t'
				else Map.empty
					
		let crsMoreHere	
		 	= if (not $ isTBot t') && (not $ resultKind kind == kValue)
				then Map.singleton t1 t'
				else Map.empty
				
		-- Add the constraints to the state.
		addCrsEq 	(Map.union crsEq'    crsEqHere)
		addCrsMore 	(Map.union crsMore'  crsMoreHere)
		addCrsOther	crsOther'
				

		
-- | If this type has an outer TConstrain node then split off the embedded constraints.	
splitTConstrain :: Type -> (Type, Map Type Type, Map Type Type, Seq Fetter)
splitTConstrain tt
 = case tt of
	TConstrain t crs
	 -> (t, crsEq crs, crsMore crs, Seq.fromList $ crsOther crs)
	
	_ -> (tt, Map.empty, Map.empty, Seq.empty)


-- | Simplify a map of more constraints.
--   The types of functions with arity > 1 tended to have repeated closure terms.
--   We don't want to trim these terms multiple times, of have duplicated information
--   in the returned type, so we make subsequent identical closure constraints
--   previous ones in the returned type.
simplifyCrsMore 
	:: Map Type Type 
	-> Map Type Type

simplifyCrsMore crs
	= Map.fromList
	$ simplifyCrsMore' [] [] 
	$ Map.toList crs

simplifyCrsMore' _ crsAcc []
	= crsAcc
	
simplifyCrsMore' crsFree crsAcc (c@(t1, t2):cs)
	| Just _	<- takeTFree t2
	= case lookup t2 crsFree of

	    -- If we've already trimmed this closure then just reuse that.
	    Just t1'	-> simplifyCrsMore' crsFree ((t1, t1') : crsAcc) cs

	    -- We haven't seen this closure term before.
	    Nothing 
	     -> let t2'	= trimClosureT t2
		    c'	= (t1, t2')

		-- The closure might have been trimmed to bottom, 
		--	if so we don't want to include a constraint for it.
	    	in if isTBot t2'
			then simplifyCrsMore' crsFree              crsAcc        cs
			else simplifyCrsMore' ((t2, t1) : crsFree) (c' : crsAcc) cs
	
	-- Some non-closure constraint.
	| otherwise
	= simplifyCrsMore' crsFree (c : crsAcc) cs
	

-- | Lookup the type of a class, converting types from node form to regular form.
--	If the type hasn't been unified yet, or if this isn't a regular equivalance
--	class then `Nothing`. The children of the nodes returned may be simple types
--	like TCon tBot, but not other types. 
takeShallowTypeOfCid :: ClassId -> TraceM (Maybe Type)
takeShallowTypeOfCid cid
 = do	cls	<- getClass cid
	takeShallowTypeOfClass cls


-- | Get the type of a `Class`.
takeShallowTypeOfClass :: Class -> TraceM (Maybe Type)
takeShallowTypeOfClass cls
 = case cls of
	Class	{ classKind		= kind
		, classTypeSources	= tsSrc
		, classUnified		= mUnified }

	 -- There are never any constructors for region classes.
	 | isRegionKind kind
	 -> return $ Just (tBot kRegion)

	 -- For effects and closures we form the result type by summing the constraints.
	 | isEffectKind kind || isClosureKind kind
	 -> do	ts	<- mapM (getTypeOfNode kind) $ map fst tsSrc
		case makeTSum kind ts of
		 TVar{}	-> return $ Just $ tBot kind
		 tSum	-> return $ Just tSum
		
	 -- For other types, we rely on the unifier to have worked out a type for us.
	 | Just nUnified	<- mUnified
	 -> do	tUnified	<- getTypeOfNode kind nUnified
		return	$ Just $ tUnified
		
	 -- Otherwise we're out of luck
	 | otherwise
	 -> freakout stage
	 	("takeShallowTypeOfClass: no type available\n" % cls)
		(return Nothing)
	
	_ -> return Nothing
		

-- | Get the `Type` corresponding to a `Node`.
getTypeOfNode 
	:: Kind 		-- ^ The kind of the node.
	-> Node 		-- ^ The node to convert.
	-> TraceM Type

getTypeOfNode kind node
 = case node of
	NBot
	 ->	return $ TSum kind []

	NVar v	
	 -> 	return $ TVar kind $ UVar v
	
	NCon tc	
	 -> 	return $ TCon tc
	
	NApp cid1 cid2
	 -> do	t1	<- loadSimpleType cid1
		t2	<- loadSimpleType cid2
		return $ TApp t1 t2
			
	NSum cids
	 -> do	ts	<- mapM loadSimpleType $ Set.toList cids
		return $ TSum kind ts
		
	NScheme t
	 -> do	t'	<- sinkCidsInType t
		return $ t'
	
	-- TODO: Get the real error here.	
	NError
	 -> 	return $ TError kind TypeError
		
	NFree v t
	 -> do	t'   	     <- sinkCidsInType t
		let Just clo = makeTFree v t'
		return clo
		

-- | If this class contains a simple type like TCon or tBot, then return that, 
--	otherwise just return the corresponding classid.
-- 
--  NOTE: We don't want to load types that contain child cids here. Those cids may be involved
--	  in loops, and its the job of the packer to work that out. 
--
--  NOTE: We also can't load constructors of effect or closure kind, as the cids may be
--	  refered to multiple times in the body of the type.
-- 
--        eg with:      (a -(!1)> b) -> a -(!1)> b :- !1 :> !Console
--
--	  Substituting !Console for !1 here would break the link between the effect of the
--	  parameter and the manifest effect of the function.
--       
loadSimpleType ::  ClassId -> TraceM Type
loadSimpleType cid
 = do	cid'		<- sinkCid cid
	classes		<- gets stateClasses
	cls		<- liftIO (readArray classes cid')
	let kind	= classKind cls

	if (kind /= kEffect && kind /= kClosure)
	 then case classUnified cls of
		Just (NCon tc)	-> return $ TCon tc
		_		-> return $ TVar kind $ UClass cid'

	 else	return	$ TVar kind $ UClass cid'


-- | Get a class from the graph.
getClass :: ClassId -> TraceM Class
getClass cid
 = do	cid'	<- sinkCid cid
	classes	<- gets stateClasses
	liftIO (readArray classes cid')
	

-- | Convert a cid to canonical form.
sinkCid :: ClassId -> TraceM ClassId
sinkCid  cid
 = do	classes	<- gets stateClasses
	liftIO (sinkCidIO classes cid)


-- | Convert cids in a type to canonical form.
sinkCidsInType :: Type -> TraceM Type
sinkCidsInType tt
 = do	classes	<- gets stateClasses	
	liftIO (sinkCidsInTypeIO classes tt)		


-- | Convert cids in a type to canonical form.
sinkCidsInFetter :: Fetter -> TraceM Fetter
sinkCidsInFetter tt
 = do	classes	<- gets stateClasses	
	liftIO (sinkCidsInFetterIO classes tt)		


-- Monad ------------------------------------------------------------------------------------------
-- | The state of the tracer.
data TraceS
	= TraceS {

	-- | The type graph.
	  stateClasses		:: IOArray ClassId Class

	-- | Cids that we've already visited.
	, stateCidsVisited	:: Set ClassId

	-- | Accumulated constraints.
	, stateAccEq		:: Map Type Type
	, stateAccMore		:: Map Type Type
	, stateAccOther		:: Seq Fetter }

type TraceM 
	= StateT TraceS IO
	
	
-- | Add an equalify constraint to the state.
addCrsEq :: Map Type Type -> TraceM ()
addCrsEq crs
	= modify $ \s -> s { 
		stateAccEq   = Map.union (stateAccEq s) crs }


-- | Add a more than constraint to the state.
addCrsMore :: Map Type Type -> TraceM ()
addCrsMore crs
	= modify $ \s -> s { 
		stateAccMore = Map.union (stateAccMore s) crs }


-- | Add some fetters to the state.
addCrsOther :: Seq Fetter -> TraceM ()
addCrsOther crs
	= modify $ \s -> s { 
		stateAccOther = (Seq.><) (stateAccOther s) crs }
	
	
-- | Run a trace computation as a squid computation.
runTraceAsSquid :: TraceM a -> SquidM a
runTraceAsSquid comp
 = do	graph		<- getsRef stateGraph
	let classes	=  graphClass graph

	let state	= TraceS
			{ stateClasses		= classes
			, stateCidsVisited	= Set.empty
			, stateAccEq		= Map.empty
			, stateAccMore		= Map.empty
			, stateAccOther		= Seq.empty }
			
	liftIO (evalStateT comp state)


	
	