
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
	
	  -- * Type graph wrappers
	, lookupClass
	, allocClass
	, updateClass
	, modifyClass
	, delFetterClass
	, activateClass
	, clearActive

	, makeClassFromVar
	, addToClass
	, foldClasses
	
	
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


-- Type Graph -------------------------------------------------------------------------------------

-- | Lookup a class from the graph.
lookupClass :: ClassId -> SquidM (Maybe Class)
lookupClass cid
 = do	graph	<- getsRef stateGraph
 	liftIO $ lookupClassFromGraph cid graph


-- | Allocate a new class into the type graph.
allocClass :: Kind -> TypeSource -> SquidM ClassId
allocClass kind src
 = do	graph		<- getsRef stateGraph
	(cid, graph')	<- liftIO $ addClassToGraph (emptyClass kind src) graph
	writesRef stateGraph graph'
	return cid


-- | Modify a class in the graph using a given function.
modifyClass :: ClassId -> (Class -> Class) -> SquidM ()
modifyClass cid f
 = do	graph	<- getsRef stateGraph
	liftIO $ modifyClassInGraph cid graph f


-- | Update a class in the graph.
updateClass :: ClassId -> Class -> SquidM ()
updateClass cid cls
 = do	graph	<- getsRef stateGraph
	liftIO $ modifyClassInGraph cid graph (\_ -> cls)
	

-- | Delete a fetter class from the type graph.
--   Note: This class *cannot* be re-used because it may have been deleted due to a MPTC
--	   being crushed out. Other nodes will still refer to this one, and Type.Trace 
--	   treats the ClassFetterDeleted as generating no constraints.
delFetterClass :: ClassId -> SquidM ()
delFetterClass cid
 = do	graph	<- getsRef stateGraph
	liftIO $ delFetterFromGraph cid graph


-- | Activate a class, and any MPTC's acting on it.
activateClass :: ClassId -> SquidM ()
activateClass cid
 = do	graph	<- getsRef stateGraph
	liftIO 	$ activateClassOfGraph cid graph


-- | Clear the set of active classes.
clearActive ::	SquidM (Set ClassId)
clearActive
 = do	graph	<- getsRef stateGraph
	liftIO	$ clearActiveClassesOfGraph graph
	

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
	  -> do	cid	<- allocClass kind src 
		addAliasForClass cid src var kind
	     	return cid




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
		 ClassUnallocated	-> update' cid (emptyClass kind src cid)
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


	

-- Fold a function through all the classes in the type graph.
foldClasses :: (a -> Class -> SquidM a) -> a -> SquidM a
foldClasses fun x
 = do  	graph		<- getsRef stateGraph
	classes		<- liftIO $ getElems $ graphClass graph
	foldM fun x classes  

				




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

