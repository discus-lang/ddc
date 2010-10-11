
-- | Squidly solver.
--   In honour of the Colossal Squid that was filmed on the day I started this.
--
module DDC.Solve.State.Squid
	( SquidM
	, SquidEnv(..)
	, SquidS(..)
	, squidSInit)
where
import qualified Shared.Unique	as U
import Constraint.Exp
import DDC.Solve.Error
import DDC.Main.Arg
import DDC.Solve.Graph
import DDC.Solve.Location
import DDC.Solve.Interface.Solution
import DDC.Solve.Interface.Problem
import DDC.Type.Data
import DDC.Type
import DDC.Var
import Data.IORef
import Control.Monad.Trans
import Control.Monad.State.Strict
import System.IO
import Data.Maybe
import Data.Set			(Set)
import Data.Map			(Map)
import qualified Data.Set	as Set
import qualified Data.Map	as Map

type SquidM	
	= StateT SquidS IO

-- | Type environment for the solver.
--   These are imported types and types of constructors, and are guaranteed not
--   to contain meta type variables.
data SquidEnv
	= SquidEnv { 
	-- | Map of type constructor name to the data type declaration for that constructor
	  squidEnvDataDefs	:: Map Var DataDef

	-- | Map of data constructor name to the type constructor for it.
	, squidEnvCtorDefs	:: Map Var CtorDef 
	
	-- | Map type var of imported binding to its type.
	, squidEnvDefs		:: Map Var ProbDef
	
	-- | Map of type var to type sigs
	, squidEnvSigs		:: Map Var [ProbSig]
	}



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

	-- | The solver environment
	, stateEnv		:: SquidEnv

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
	, stateGenSusp		:: IORef (Map Var TypeSource)

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
squidSInit 
	:: [Arg]		-- ^ Compiler arguments.
	-> Maybe Handle		-- ^ File handle to dump solver trace.
	-> Problem		-- ^ Typing problem to solve.
	-> IO SquidS

squidSInit args mTrace problem
 = do	
	-- Variable generator stuff
	let Just tT	= lookup NameType 	U.typeSolve
	let Just rT	= lookup NameRegion 	U.typeSolve
	let Just eT	= lookup NameEffect	U.typeSolve
	let Just cT 	= lookup NameClosure	U.typeSolve
   
	let varGen	= Map.insert NameType    (VarId tT 0)
			$ Map.insert NameRegion  (VarId rT 0)
			$ Map.insert NameEffect  (VarId eT 0)
			$ Map.insert NameClosure (VarId cT 0)
			$ Map.empty 

	refTraceIndent	<- liftIO $ newIORef 0
	refSigmaTable	<- liftIO $ newIORef $ problemValueToTypeVars  problem
	refVsBoundTop	<- liftIO $ newIORef $ problemTopLevelTypeVars problem
	refVarGen	<- liftIO $ newIORef varGen
	refVarSub	<- liftIO $ newIORef Map.empty

	-- Solver environment
	let ctorDataMap	= Map.fromList
			$ [(ctorNameT, ctorDef)
				| dataDef	<- Map.elems $ problemDataDefs problem
				, ctorDef	<- Map.elems $ dataDefCtors dataDef
				, ctorNameT	<- maybeToList $ Map.lookup (ctorDefName ctorDef) 
							(problemValueToTypeVars problem) ]
	let squidEnv
		= SquidEnv
		{ squidEnvDataDefs	= problemDataDefs problem
		, squidEnvCtorDefs	= ctorDataMap 
		, squidEnvDefs		= problemDefs problem 
		, squidEnvSigs		= problemSigs problem }
			
	-- The type graph
	graph		<- makeEmptyGraph
   	refGraph	<- liftIO $ newIORef graph
   
	-- Solver state
	refPath		<- liftIO $ newIORef []
	refContains	<- liftIO $ newIORef Map.empty
	refInstantiates	<- liftIO $ newIORef Map.empty
	refGenSusp	<- liftIO $ newIORef Map.empty
	refGenDone	<- liftIO $ newIORef Set.empty
	refGenInst	<- liftIO $ newIORef Map.empty
	refQuantVsKM	<- liftIO $ newIORef Map.empty
	refQuantVs	<- liftIO $ newIORef Set.empty
	refProject	<- liftIO $ newIORef Map.empty
	refProjResolve	<- liftIO $ newIORef Map.empty
	refClassInst	<- liftIO $ newIORef Map.empty

   	return	SquidS
		{ stateTrace		= mTrace
		, stateErrors		= []
		, stateStop		= False 
		, stateArgs		= Set.fromList args
		, stateEnv		= squidEnv
		, stateTraceIndent	= refTraceIndent
		, stateSigmaTable	= refSigmaTable
		, stateVsBoundTopLevel	= refVsBoundTop
		, stateVarGen		= refVarGen
		, stateVarSub		= refVarSub
		, stateGraph		= refGraph
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

