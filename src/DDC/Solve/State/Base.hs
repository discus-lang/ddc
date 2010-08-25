
module DDC.Solve.State.Base
	( -- * IORef horror show
	  getsRef
	, writesRef
	, modifyRef

	  -- * Tracing
	, traceM
	, traceI
	, traceIE
	, traceIL

	  -- * Error Handling
	, addErrors
	, gotErrors

	  -- * Path Management
	, pathEnter
	, pathLeave 
	
	  -- * Equivalence Classes
	, expandGraph
	, allocClass
	, makeClassFromVar
	, delClass
	, addToClass
	, lookupClass
	, updateClass
	, modifyClass
	, foldClasses
	
	  -- * Activity Queue
	, clearActive
	, activateClass
	
	  -- * Bits and pieces
	, lookupSourceOfNode
	, deleteSingleFetter
	, graphInstantiatesAdd
	, kindOfCid)
where
import Constraint.Exp
import Constraint.Pretty	()
import Type.Location
import Type.Error
import DDC.Solve.State.Class
import DDC.Solve.State.Graph
import DDC.Solve.State.Squid
import DDC.Solve.State.Node
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Type
import DDC.Var
import Control.Monad.State.Strict
import System.IO
import Data.IORef
import Data.Array.IO
import Data.Maybe
import qualified DDC.Main.Arg	as Arg
import qualified Data.Set	as Set
import qualified Data.Map	as Map
import qualified Util.Data.Map	as Map
import Util
import {-# SOURCE #-} DDC.Solve.State.Naming
import {-# SOURCE #-} DDC.Solve.State.Sink

stage	= "DDC.Solve.State.Base"

-- IORef horror show ------------------------------------------------------------------------------
getsRef :: (SquidS -> IORef a) -> SquidM a
{-# INLINE getsRef #-}
getsRef getRef
 = do	ref	<- gets getRef
	liftIO	$ readIORef ref

writesRef :: (SquidS -> IORef a) -> a -> SquidM ()
{-# INLINE writesRef #-}
writesRef getRef x
 = do	ref	<- gets getRef
	liftIO	$ writeIORef ref x

modifyRef :: (SquidS -> IORef a) -> (a -> a) -> SquidM ()
{-# INLINE modifyRef #-}
modifyRef getRef fn
 = do	ref	<- gets getRef
	liftIO	$ modifyIORef ref fn


-- Tracing ----------------------------------------------------------------------------------------
-- | Add some stuff to the inferencer trace.
traceM :: PrettyM PMode -> SquidM ()
traceM p
 = do	mHandle	<- gets stateTrace
	i	<- getsRef stateTraceIndent
	args	<- gets stateArgs
 	case mHandle of
	 Nothing	-> return ()
	 Just handle
	  -> do 
	  	liftIO (hPutStr handle $ indentSpace i 
				$ pprStr (catMaybes $ map Arg.takePrettyModeOfArg $ Set.toList args) p)
	  	liftIO (hFlush  handle)

	
-- | Do some solver thing, while indenting anything it adds to the trace.
traceI :: SquidM a -> SquidM a
traceI fun
 = do	traceIE
 	x	<- fun
	traceIL
	return x

traceIE :: SquidM ()
traceIE	= stateTraceIndent `modifyRef` \i -> i + 4
 
traceIL :: SquidM ()
traceIL	= stateTraceIndent `modifyRef` \i -> i - 4


-- Error Handling --------------------------------------------------------------------------------- 
-- | Add some errors to the monad.
--	These'll be regular user-level type errors from the compiled program.
addErrors ::	[Error]	-> SquidM ()
addErrors	errs
	= modify (\s -> s { stateErrors = stateErrors s ++ errs })


-- | See if there are any errors in the state
gotErrors :: SquidM Bool
gotErrors
 = do	errs	<- gets stateErrors
 	return	$ not $ isNil errs


-- Path Management --------------------------------------------------------------------------------
-- | Push a new var on the path queue.
--	This records the fact that we've entered a branch.
pathEnter :: CBind -> SquidM ()
pathEnter BNothing	= return ()
pathEnter v	
	= statePath `modifyRef` \path -> v : path 


-- | Pop a var off the path queue.
--	This records the fact that we've left the branch.
pathLeave :: CBind -> SquidM ()
pathLeave BNothing	= return ()
pathLeave bind
  = statePath `modifyRef` \path ->
	case path of
	  	-- pop matching binders off the path
		b1 : bs
		 | bind == b1	-> bs
	
		-- nothing matched.. :(
		_ -> panic stage $ "pathLeave: can't leave " % bind % "\n"


-- Class Allocation -------------------------------------------------------------------------------
-- | Increase the size of the type graph.
expandGraph 
	:: Int 			-- ^ Graph must have this many free nodes after expansion.
	-> SquidM ()

expandGraph minFree
 = do	graph			<- getsRef stateGraph
 	ClassId curMax		<- liftM snd $ liftIO $ getBounds (graphClass graph)
	let curIx		= graphClassIdGen graph
	
	if curIx + minFree <= curMax
	 then return ()
	 else do 
	 	let newMax	= curMax * 2

		elems		<- liftIO (getElems (graphClass graph))
		newClass	<- liftIO 
					(newListArray (ClassId 0, ClassId newMax)
					(elems ++ replicate curMax ClassUnallocated))

		stateGraph `modifyRef` \graph' -> 
			graph' { graphClass = newClass }

		return ()
 	

-- | Allocate a new class in the type graph.
allocClass 	
	:: TypeSource
	-> Kind			-- The kind of the class.
	-> SquidM ClassId

allocClass src kind
 = do	expandGraph	1

	graph		<- getsRef stateGraph
	let classIdGen	=  graphClassIdGen graph
 	let cid		= ClassId classIdGen

	liftIO 	$ writeArray (graphClass graph) cid
		$ classEmpty cid kind src

	stateGraph `modifyRef` \graph2 -> 
	 	graph2 { graphClassIdGen	= classIdGen + 1}

	return cid


-- | If there is already a class for this variable then return that
--   otherwise make a new one containing this var.
makeClassFromVar
	:: TypeSource		-- ^ Source of the constraint containing the variable.
	-> Kind			-- ^ Kind of this variable.
	-> Var			-- ^ The variable.
	-> SquidM ClassId
		
makeClassFromVar src kind var
 = do	mCid		<- lookupVarToClassId var
   	case mCid of
   	 Just cid	-> return cid
	 Nothing 
	  -> do	cid	<- allocClass src kind
		addAliasForClass cid src var kind
	     	return cid


-- | Delete a class by setting it to Nil.
--   Note: This class *cannot* be re-used because it may have been deleted due to a MPTC
--	   being crushed out. Other nodes will still refer to this one, and Type.Trace 
--	   treats the ClassFetterDeleted as generating no constraints.
delClass :: ClassId -> SquidM ()
delClass cid
 = do	Just cls	<- lookupClass cid
	case cls of
	 ClassFetter{}	
	  -> do	updateClass cid (ClassFetterDeleted cls)
		return ()
		
	 _ ->	panic stage $ "delClass: class " % cid % " to be deleted is not a ClassFetter{}"
	



-- | Add a new type constraint to a class
--	Doing this makes the class active.
addToClass 
	:: ClassId		-- ^ id of class to update
	-> TypeSource		-- ^ source of constraint
	-> Kind			-- ^ kind of the constraint
	-> Node			-- ^ constraint
	-> SquidM ()

addToClass cid src kind node
 = do	graph		<- getsRef stateGraph
	addToClass2 cid src kind node graph

addToClass2 cid' src kind node graph
 = go cid'
 where	go cid
	 = do	cls	<- liftIO (readArray (graphClass graph) cid)
		case cls of
		 ClassForward _ cid'' 	-> go cid''
		 ClassUnallocated	-> update' cid (classEmpty cid kind src)
		 Class{}		-> update' cid cls
		 	
	update' cid cls@Class{}
	 = do	liftIO 	$ writeArray (graphClass graph) cid 
			$ cls 	{ classUnified		= Nothing
				, classTypeSources	= (node, src) : classTypeSources cls }
			
		activateClass cid
		linkVar cid node
		
	linkVar cid tt
 	 = case tt of
 		NVar v
	 	 -> do	stateGraph `modifyRef` \graph' -> 
				graph' { graphVarToClassId = Map.insert v cid (graphVarToClassId graph) }

		_ -> return ()


-- | Lookup a class from the graph.
lookupClass 
	:: ClassId 
	-> SquidM (Maybe Class)

lookupClass cid_ 
 = do	cid	<- sinkClassId cid_
	graph	<- getsRef stateGraph
	c	<- liftIO (readArray (graphClass graph) cid)
	return $ Just c



-- | Update a class in the graph.
updateClass	
	:: ClassId 		-- ^ id of class to update.
	-> Class  		-- ^ new class.
	-> SquidM ()

updateClass cid_ c
 = do	cid		<- sinkClassId cid_
 	graph		<- getsRef stateGraph
	liftIO (writeArray (graphClass graph) cid c)
	return ()


-- | Modify a class in the graph using this modification function
modifyClass
	:: ClassId
	-> (Class -> Class)
	-> SquidM ()
	
modifyClass cid_ f
 = do	cid	<- sinkClassId cid_
 	graph	<- getsRef stateGraph
	c	<- liftIO (readArray (graphClass graph) cid)
	liftIO (writeArray (graphClass graph) cid (f c))
	return ()
	

-- Fold a function through all the classes in the type graph.
foldClasses :: (a -> Class -> SquidM a) -> a -> SquidM a
foldClasses fun x
 = do  	graph		<- getsRef stateGraph
	classes		<- liftIO $ getElems $ graphClass graph
	foldM fun x classes  

				
-- Activity Queue ---------------------------------------------------------------------------------
-- | Clear the set of active classes.
clearActive ::	SquidM (Set ClassId)
clearActive
 = do	graph	<- getsRef stateGraph 
	
	active'	<- liftM Set.fromList
		$  mapM sinkClassId 
		$  Set.toList
		$  graphActive graph
	
	stateGraph `modifyRef` \graph' ->
		graph' { graphActive = Set.empty }

	return	active'


-- | Activate a class, tagging it for inspection by the unifier \/ crusher.
--	Also activate any MPTCs acting on it.
activateClass :: ClassId -> SquidM ()
activateClass cid
 = do	-- traceM $ "activating class " % cid % "\n"

	stateGraph `modifyRef` \graph -> 
		graph { graphActive = Set.insert cid (graphActive graph) }
		
	Just c		<- lookupClass cid
	(case c of
		Class { classFettersMulti = cidsMulti}
		 	-> mapM_ activateClass $ Set.toList cidsMulti
		_	-> return ())


-- Bits and Pieces -------------------------------------------------------------------------------
-- | Get the source of some effect, given the class that contains it.
--	The cids in the provided effect must be in canonical form, 
--	but the cids in the class don't need to be.
--	If there are multiple sources in the class then just take the first one.
lookupSourceOfNode
	:: Node
	-> Class 
	-> SquidM (Maybe TypeSource)

lookupSourceOfNode nEff cls
 = do	tsSrcs	<- mapM sinkCidsInNodeFst $ classTypeSources cls
	return 	$ listToMaybe
		$ [nodeSrc	| (nodeEff,  nodeSrc)	<- tsSrcs
				, nodeEff == nEff]

-- | Delete a SPTC Fetter from a class.
deleteSingleFetter
	:: ClassId 
	-> Var
	-> SquidM ()
	
deleteSingleFetter cid v
 = do	Just cls	<- lookupClass cid
	let cls'	= cls { classFetters = Map.delete v (classFetters cls) }
	updateClass cid cls'
	
		
-- | Add to the who instantiates who list
graphInstantiatesAdd :: CBind -> CBind -> SquidM ()
graphInstantiatesAdd    vBranch vInst
 = stateInstantiates `modifyRef` \instantiates -> 
	Map.adjustWithDefault 
		(Set.insert vInst) 
		Set.empty
		vBranch
		instantiates


-- lookup the kind of the class corresponding to this var.
kindOfCid :: ClassId -> SquidM Kind
kindOfCid cid
 = do	Just c	<- lookupClass cid
 	return	$ classKind c

