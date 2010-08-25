
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
	
	
	  -- * Bits and pieces
	, lookupSourceOfNode
	, graphInstantiatesAdd)
where
import Constraint.Exp
import Constraint.Pretty	()
import Type.Location
import Type.Error
import DDC.Solve.Graph
import DDC.Solve.State.Squid
import DDC.Main.Error
import DDC.Main.Pretty
import Control.Monad.State.Strict
import System.IO
import Data.IORef
import Data.Maybe
import qualified DDC.Main.Arg	as Arg
import qualified Data.Set	as Set
import qualified Util.Data.Map	as Map
import Util
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

			
-- | Add to the who instantiates who list
graphInstantiatesAdd :: CBind -> CBind -> SquidM ()
graphInstantiatesAdd    vBranch vInst
 = stateInstantiates `modifyRef` \instantiates -> 
	Map.adjustWithDefault 
		(Set.insert vInst) 
		Set.empty
		vBranch
		instantiates



