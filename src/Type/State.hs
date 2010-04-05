-- | Type inferencer state.
module Type.State
	( SquidM
	, SquidS (..)
	, squidSInit
	, module Type.Base
	, traceM, traceI, traceIE, traceIL
	, instVar
	, newVarN
	, lookupSigmaVar
	, addErrors 
	, gotErrors
	, pathEnter
	, pathLeave
	, graphInstantiatesAdd )
where
import Constraint.Exp
import Type.Error
import Type.Base
import Type.Exp
import Util
import System.IO
import DDC.Solve.InstanceInfo
import DDC.Var
import DDC.Main.Pretty
import DDC.Main.Error
import Constraint.Pretty	()
import DDC.Main.Arg		(Arg)
import qualified DDC.Main.Arg	as Arg
import qualified Shared.Unique	as U
import qualified Data.Map	as Map
import qualified Util.Data.Map	as Map
import qualified Data.Set	as Set

-----
stage	= "Type.State"

-----
type SquidM	= StateT SquidS IO

data SquidS 
	= SquidS
	{ 
	-- | Where to write the trace of what the solver's doing.
	  stateTrace		:: Maybe Handle			
	, stateTraceIndent	:: Int

	-- | Errors encountered whilst solving the constraints.
	, stateErrors		:: [Error]

	-- | Signals that we should stop the solver and not process the next constraint
	--	Useful during debugging, not used otherwise.
	, stateStop		:: Bool 

	-- | The args from the command line
	, stateArgs		:: Set Arg	

	-- | Map of value variables to type variables.
	, stateSigmaTable	:: Map Var Var

	-- | Type vars of value vars bound at top level.
	--	Free regions in the types of top level bindings default to be constant.
	, stateVsBoundTopLevel	:: Set Var
	
	-- | New variable generator.
	, stateVarGen		:: Map NameSpace VarId

	-- | Variable substitution.	
	, stateVarSub		:: Map Var	 Var 

	-- | The type graph
	, stateGraph		:: Graph

	-- | Type definitons from CDef constraints.
	--	Most of these won't be used in any particular program and we don't want to pollute
	--	the type graph with this information, nor have to extract them back from the graph
	--	when it's time to export types to the Desugar->Core transform.
	, stateDefs		:: Map Var Type

	-- | The current path we've taken though the branches.
	--	This tells us what branch we're currently in, and by tracing
	--	through the path we can work out how a particular variable was bound.
	, statePath		:: [ CBind ]

	-- | Which branches contain \/ instantiate other branches.
	--	This is used to work out what bindings are part of recursive groups, 
	--	and to determine the type environment for a particular branch when it's 
	--	time to generalise it.
	, stateContains		:: Map CBind	(Set CBind)
	, stateInstantiates	:: Map CBind 	(Set CBind)

	-- | Vars of types which are waiting to be generalised.
	--	We've seen a CGen telling us that all the constraints for the type are in 
	--	the graph, but we haven't done the generalisation yet. If this binding is
	--	part of a recursive group then it won't be safe to generalise it until we're
	--	out of that group.
	, stateGenSusp		:: Set Var

	-- | Vars of types which have already been generalised 
	--	When we want to instantiate the type for one of the vars in this set then
	--	we can just extract it from the graph, nothing more to do.
	, stateGenDone		:: Set Var

	-- | Records how each scheme was instantiated.
	--	We need this to reconstruct the type applications during conversion to
	--	the Core IR.
	--	
	, stateInst		:: Map Var (InstanceInfo Var)			

	-- | Records what vars have been quantified. (with optional :> bounds)
	--	After the solver is finished and all generalisations have been performed,
	--	all effect and closure ports will be in this set. We can then clean out
	--	non-ports while we extract them from the graph.
	, stateQuantifiedVarsKM	:: Map Var (Kind, Maybe Type)

	-- | We sometimes need just a set of quantified vars, 
	--	and maintaining this separately from the above stateQuanfiedVarsFM is faster.
	, stateQuantifiedVars	:: Set Var
									
	-- | The projection dictionaries
	--	ctor name -> (type, field var -> implemenation var)
	, stateProject		:: Map Var	(Type, Map Var Var)	
	
	-- | When projections are resolved, Crush.Proj adds an entry to this table mapping the tag
	--	var in the constraint to the instantiation var. We need this in Desugar.ToCore to rewrite
	--	projections to the appropriate function call.
	, stateProjectResolve	:: Map Var Var
									
	-- | Instances for type classses
	--	class name -> instances for this class.
	--   eg Num	   -> [Num (Int %_), Num (Int32# %_)]
	, stateClassInst	:: Map Var 	[Fetter] }


-- | build an initial solver state
squidSInit :: IO SquidS
squidSInit
 = do	let Just tT	= lookup NameType 	U.typeSolve
	let Just rT	= lookup NameRegion 	U.typeSolve
	let Just eT	= lookup NameEffect	U.typeSolve
	let Just cT 	= lookup NameClosure	U.typeSolve
   
   	graph		<- graphInit
   
   	return	SquidS
		{ stateTrace		= Nothing
		, stateTraceIndent	= 0
		, stateArgs		= Set.empty
		, stateSigmaTable	= Map.empty
		, stateVsBoundTopLevel	= Set.empty

		, stateVarGen		= Map.insert NameType    (VarId tT 0)
					$ Map.insert NameRegion  (VarId rT 0)
					$ Map.insert NameEffect  (VarId eT 0)
					$ Map.insert NameClosure (VarId cT 0)
					$ Map.empty 

		, stateVarSub		= Map.empty 
		, stateGraph		= graph
		, stateDefs		= Map.empty
		, statePath		= []
		, stateContains		= Map.empty
		, stateInstantiates	= Map.empty
		, stateGenSusp		= Set.empty
		, stateGenDone		= Set.empty
		, stateInst		= Map.empty
		, stateQuantifiedVarsKM	= Map.empty
		, stateQuantifiedVars	= Set.empty
		, stateProject		= Map.empty
		, stateProjectResolve	= Map.empty
		, stateClassInst	= Map.empty
		, stateErrors		= []
		, stateStop		= False }


-- | Add some stuff to the inferencer trace.
traceM :: PrettyM PMode -> SquidM ()
traceM p
 = do	mHandle	<- gets stateTrace
	i	<- gets stateTraceIndent
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
traceIE
 = modify (\s -> s { stateTraceIndent = stateTraceIndent s + 4 })
 
traceIL :: SquidM ()
traceIL
 = modify (\s -> s { stateTraceIndent = stateTraceIndent s - 4 })
 
 
-- | Instantiate a variable.
instVar :: Var -> SquidM (Maybe Var)
instVar var
 = do	let space	= varNameSpace var

	-- lookup the generator for this namespace
	mVarId		<- liftM (Map.lookup space) $ gets stateVarGen
	instVar' var space mVarId

instVar' var space mVarId
  	| Nothing	<- mVarId
	= freakout stage
	  	("instVar: can't instantiate var in space " % show space
	  	% " var = " % show var)
		$ return Nothing
		
	| Just vid	<- mVarId
	= do
		-- increment the generator and write it back into the table.
		let vid'	= incVarId vid
		modify $ \s -> s { stateVarGen = Map.insert space vid' (stateVarGen s) }

		-- the new variable remembers what it's an instance of..
		let name	= pprStrPlain vid
		let var'	= (varWithName name)
			 	{ varNameSpace	= varNameSpace var
			 	, varId		= vid }

		return $ Just var'


-- | Make a new variable in this namespace
newVarN :: NameSpace ->	SquidM Var
newVarN	space	
 = do
 	Just vid	<- liftM (Map.lookup space)
			$  gets stateVarGen
	
	let vid'	= incVarId vid
	modify $ \s -> s { stateVarGen = Map.insert space vid' (stateVarGen s) }
	
	let name	= pprStrPlain vid
	let var'	= (varWithName name)
			{ varNameSpace	= space 
			, varId		= vid }
			
	return var'


-- | Lookup the type variable corresponding to this value variable.
lookupSigmaVar :: Var -> SquidM (Maybe Var)
lookupSigmaVar	v
 	= liftM (Map.lookup v)
	$ gets stateSigmaTable
	
	
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


-- | Push a new var on the path queue.
--	This records the fact that we've entered a branch.
pathEnter :: CBind -> SquidM ()
pathEnter BNil	= return ()
pathEnter v
 = modify (\s -> s { statePath = v : statePath s })


-- | Pop a var off the path queue
--	This records the fact that we've left the branch.
pathLeave :: CBind -> SquidM ()
pathLeave BNil	= return ()
pathLeave bind
 = do	path	<- gets statePath

 	let (res :: SquidM ())
		-- pop matching binders off the path
		| b1 : bs		<- path
		, bind == b1
		= modify $ \s -> s { statePath = bs }
	
		-- nothing matched.. :(
		| otherwise
		= panic stage
		 	$ "pathLeave: can't leave " % bind % "\n"
--			% "  path = " % path % "\n"
	res
		
-- | Add to the who instantiates who list
graphInstantiatesAdd :: CBind -> CBind -> SquidM ()
graphInstantiatesAdd    vBranch vInst
 = modify (\s -> s {
 	stateInstantiates
		= Map.adjustWithDefault 
			(Set.insert vInst) 
			Set.empty
			vBranch
			(stateInstantiates s) })

