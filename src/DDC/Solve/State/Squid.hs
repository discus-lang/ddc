
-- | Squidly solver.
--   In honour of the Colossal Squid that was filmed on the day I started this.
--
module DDC.Solve.State.Squid
	( SquidM
	, SquidS(..)
	, squidSInit)
where
import qualified Shared.Unique	as U
import Constraint.Exp
import Type.Error
import DDC.Main.Arg
import DDC.Solve.Graph
import DDC.Solve.State.InstanceInfo
import DDC.Type
import DDC.Var
import Data.IORef
import Control.Monad.Trans
import Control.Monad.State.Strict
import System.IO
import Data.Set			(Set)
import Data.Map			(Map)
import qualified Data.Set	as Set
import qualified Data.Map	as Map

type SquidM	
	= StateT SquidS IO

data SquidS 
	= SquidS
	{ 
	-- Constantish stuff -----------------------------
	-- | Where to write the trace of what the solver's doing.
	  stateTrace		:: Maybe Handle			

	-- | Errors encountered whilst solving the constraints.
	, stateErrors		:: [Error]

	-- | Signals that we should stop the solver and not process the next constraint
	--	Useful during debugging, not used otherwise.
	, stateStop		:: Bool 

	-- | The args from the command line
	, stateArgs		:: Set Arg	

        -- Mutable stuff ----------------------------------
	-- | Current indent level for the trace
	, stateTraceIndent	:: IORef Int

	-- | Map of value variables to type variables.
	, stateSigmaTable	:: IORef (Map Var Var)

	-- | Type vars of value vars bound at top level.
	--	Free regions in the types of top level bindings default to be constant.
	, stateVsBoundTopLevel	:: IORef (Set Var)
	
	-- | New variable generator.
	, stateVarGen		:: IORef (Map NameSpace VarId)

	-- | Variable substitution.	
	, stateVarSub		:: IORef (Map Var Var)

	-- | The type graph
	, stateGraph		:: IORef Graph

	-- | Type definitons from CDef constraints.
	--	Most of these won't be used in any particular program and we don't want to pollute
	--	the type graph with this information, nor have to extract them back from the graph
	--	when it's time to export types to the Desugar->Core transform.
	, stateDefs		:: IORef (Map Var Type)

	-- | The current path we've taken though the branches.
	--	This tells us what branch we're currently in, and by tracing
	--	through the path we can work out how a particular variable was bound.
	, statePath		:: IORef [CBind]

	-- | Which branches contain \/ instantiate other branches.
	--	This is used to work out what bindings are part of recursive groups, 
	--	and to determine the type environment for a particular branch when it's 
	--	time to generalise it.
	, stateContains		:: IORef (Map CBind (Set CBind))
	, stateInstantiates	:: IORef (Map CBind (Set CBind))

	-- | Vars of types which are waiting to be generalised.
	--	We've seen a CGen telling us that all the constraints for the type are in 
	--	the graph, but we haven't done the generalisation yet. If this binding is
	--	part of a recursive group then it won't be safe to generalise it until we're
	--	out of that group.
	, stateGenSusp		:: IORef (Set Var)

	-- | Vars of types which have already been generalised 
	--	When we want to instantiate the type for one of the vars in this set then
	--	we can just extract it from the graph, nothing more to do.
	, stateGenDone		:: IORef (Set Var)

	-- | Records how each scheme was instantiated.
	--	We need this to reconstruct the type applications during conversion to
	--	the Core IR.
	, stateInst		:: IORef (Map Var (InstanceInfo Var))

	-- | Records what vars have been quantified. (with optional :> bounds)
	--	After the solver is finished and all generalisations have been performed,
	--	all effect and closure ports will be in this set. We can then clean out
	--	non-ports while we extract them from the graph.
	, stateQuantifiedVarsKM	:: IORef (Map Var (Kind, Maybe Type))

	-- | We sometimes need just a set of quantified vars, 
	--	and maintaining this separately from the above stateQuanfiedVarsFM is faster.
	, stateQuantifiedVars	:: IORef (Set Var)
									
	-- | The projection dictionaries
	--	ctor name -> (type, field var -> implemenation var)
	, stateProject		:: IORef (Map Var (Type, Map Var Var))
	
	-- | When projections are resolved, Crush.Proj adds an entry to this table mapping the tag
	--	var in the constraint to the instantiation var. We need this in Desugar.ToCore to rewrite
	--	projections to the appropriate function call.
	, stateProjectResolve	:: IORef (Map Var Var)
									
	-- | Instances for type classses
	--	class name -> instances for this class.
	--   eg Num	   -> [Num (Int %_), Num (Int32# %_)]
	, stateClassInst	:: IORef (Map Var [Fetter]) }


-- | build an initial solver state
squidSInit :: IO SquidS
squidSInit
 = do	let Just tT	= lookup NameType 	U.typeSolve
	let Just rT	= lookup NameRegion 	U.typeSolve
	let Just eT	= lookup NameEffect	U.typeSolve
	let Just cT 	= lookup NameClosure	U.typeSolve
   
	let varGen	= Map.insert NameType    (VarId tT 0)
			$ Map.insert NameRegion  (VarId rT 0)
			$ Map.insert NameEffect  (VarId eT 0)
			$ Map.insert NameClosure (VarId cT 0)
			$ Map.empty 

	refTraceIndent	<- liftIO $ newIORef 0
	refSigmaTable	<- liftIO $ newIORef Map.empty
	refVsBoundTop	<- liftIO $ newIORef Set.empty
	refVarGen	<- liftIO $ newIORef varGen
	refVarSub	<- liftIO $ newIORef Map.empty

	graph		<- makeEmptyGraph
   	refGraph	<- liftIO $ newIORef graph
   
	refDefs		<- liftIO $ newIORef Map.empty
	refPath		<- liftIO $ newIORef []
	refContains	<- liftIO $ newIORef Map.empty
	refInstantiates	<- liftIO $ newIORef Map.empty
	refGenSusp	<- liftIO $ newIORef Set.empty
	refGenDone	<- liftIO $ newIORef Set.empty
	refGenInst	<- liftIO $ newIORef Map.empty
	refQuantVsKM	<- liftIO $ newIORef Map.empty
	refQuantVs	<- liftIO $ newIORef Set.empty
	refProject	<- liftIO $ newIORef Map.empty
	refProjResolve	<- liftIO $ newIORef Map.empty
	refClassInst	<- liftIO $ newIORef Map.empty

   	return	SquidS
		{ stateTrace		= Nothing
		, stateErrors		= []
		, stateStop		= False 
		, stateArgs		= Set.empty
		, stateTraceIndent	= refTraceIndent
		, stateSigmaTable	= refSigmaTable
		, stateVsBoundTopLevel	= refVsBoundTop
		, stateVarGen		= refVarGen
		, stateVarSub		= refVarSub
		, stateGraph		= refGraph
		, stateDefs		= refDefs
		, statePath		= refPath
		, stateContains		= refContains
		, stateInstantiates	= refInstantiates
		, stateGenSusp		= refGenSusp
		, stateGenDone		= refGenDone
		, stateInst		= refGenInst
		, stateQuantifiedVarsKM	= refQuantVsKM
		, stateQuantifiedVars	= refQuantVs
		, stateProject		= refProject
		, stateProjectResolve	= refProjResolve
		, stateClassInst	= refClassInst }

