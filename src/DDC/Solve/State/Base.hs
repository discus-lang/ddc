
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

	  -- * Naming
	, newVarN
	, lookupSigmaVar
	
	  -- * Path Management
	, pathEnter
	, pathLeave 
	
	  -- * Bits and pieces
	, graphInstantiatesAdd)
where
import DDC.Constraint.Exp
import DDC.Constraint.Pretty	()
import DDC.Solve.Error
import DDC.Solve.State.Squid
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Var
import Data.IORef
import Data.Maybe
import System.IO
import Control.Monad.State.Strict
import qualified DDC.Main.Arg	as Arg
import qualified Data.Set	as Set
import qualified Data.MapUtil	as Map
import Util

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
traceM :: Str -> SquidM ()
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


-- Naming ----------------------------------------------------------------------------------------
-- | Make a new variable in this namespace
newVarN :: NameSpace ->	SquidM Var
newVarN	space	
 = do 	Just vid	<- liftM (Map.lookup space)
			$  getsRef stateVarGen
	
	let vid'	= incVarId vid

	stateVarGen `modifyRef` \varGen -> 
		Map.insert space vid' varGen
	
	let name	= pprStrPlain vid
	let var'	= (varWithName name)
			{ varNameSpace	= space 
			, varId		= vid }
			
	return var'


-- | Lookup the type variable corresponding to this value variable.
lookupSigmaVar :: Var -> SquidM (Maybe Var)
lookupSigmaVar	v
 	= liftM (Map.lookup v)
	$ getsRef stateSigmaTable


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


-- Bits and Pieces -------------------------------------------------------------------------------

-- | Add to the who instantiates who list
graphInstantiatesAdd :: CBind -> CBind -> SquidM ()
graphInstantiatesAdd    vBranch vInst
 = stateInstantiates `modifyRef` \instantiates -> 
	Map.adjustWithDefault 
		(Set.insert vInst) 
		Set.empty
		vBranch
		instantiates

