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
	
	-- get/set projections.
	, sTrace
	, sSigmaTable
	, sVarGen
	, sVarSub
	, sInst
	, sDataFields )
where

-----
import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import Shared.Error
import Shared.Var		(Var, VarBind, NameSpace(..))
import qualified Shared.Var	as Var

import qualified Shared.Unique	as U

import qualified Main.Arg	as Arg
import Main.Arg			(Arg)

import Type.Exp
import Type.Base
import Type.Error
import Constraint.Exp

import System.IO

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

	-- | New variable generator.
	, stateVarGen		:: Map NameSpace VarBind 

	-- | Variable substitution.	
	, stateVarSub		:: Map Var	 Var 

	-- | The type graph
	, stateGraph		:: Graph

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

	-- | Vars of types which have already been generalised (or imported from another module)
	--	When we want to instantiate the type for one of the vars in this set then
	--	we can just extract it from the graph, nothing more to do.
	, stateGenDone		:: Set Var

	-- | Records how each scheme was instantiated.
	--	We need this to reconstruct the type applications during conversion to
	--	the Core IR.
	--	
	, stateInst		:: Map Var	(InstanceInfo Var Type)			

	-- | Records the port-forcing table for each generalised type.
	--	We'll need this to rewrite the Core IR to use the new names for type varibles
	--	introduced by the port-forcing process.
	, statePortTable	:: Map Var	(Map Var Type)

	-- | Records what vars have been quantified.
	--	After the solver is finished and all generalisations have been performed,
	--	all effect and closure ports will be in this set. We can then clean out
	--	non-ports while we extract them from the graph.
	, stateQuantifiedVars	:: Set Var
	
	-- | A register of which classes contain various, interesting effect \/ class constructors.
	--	When we want to crush out some of the class constraints, we can use this register
	--	to find them, instead of searching through the whole group.
	, stateRegister		:: Map VarBind  (Set ClassId)		
									
	-- | The data field definitions.
	-- 	type name	-> (type vars, [(field name, field type)])
	, stateDataFields	:: Map Var	([Var], [(Var, Type)]) 
								
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

		, stateVarGen		= Map.insert NameType    (Var.XBind tT 0)
					$ Map.insert NameRegion  (Var.XBind rT 0)
					$ Map.insert NameEffect  (Var.XBind eT 0)
					$ Map.insert NameClosure (Var.XBind cT 0)
					$ Map.empty 
				
		, stateVarSub		= Map.empty 

		, stateGraph		= graph
	
		, statePath		= []

		, stateContains		= Map.empty
		, stateInstantiates	= Map.empty
	
		, stateGenSusp		= Set.empty
		, stateGenDone		= Set.empty

		, stateInst		= Map.empty
		, statePortTable	= Map.empty
		, stateQuantifiedVars	= Set.empty

		, stateRegister		
			-- effects
			= Map.insert Var.EReadH 	Set.empty
			$ Map.insert Var.EReadT		Set.empty
			$ Map.insert Var.EWriteT	Set.empty
	
			-- fetters
			$ Map.insert Var.FLazyH 	Set.empty
			$ Map.insert Var.FMutableT	Set.empty
			$ Map.insert Var.FConstT 	Set.empty

			$ Map.insert (Var.FShape 0) 	Set.empty
			$ Map.insert Var.FProj	 	Set.empty

			$ Map.empty

		, stateDataFields	= Map.empty 
		, stateProject		= Map.empty
		, stateProjectResolve	= Map.empty
		, stateClassInst	= Map.empty
		, stateErrors		= []
		, stateStop		= False }


-- | Add some stuff to the inferencer trace.
traceM :: PrettyP -> SquidM ()
traceM p
 = do	mHandle		<- gets stateTrace
	i		<- gets stateTraceIndent
 	case mHandle of
	 Nothing	-> return ()
	 Just handle
	  -> do liftIO (hPutStr handle $ indent i $ pretty p)
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
 = do	let space	= Var.nameSpace var

	-- lookup the generator for this namespace
	mVarId		<- liftM (Map.lookup space) $ gets stateVarGen
	instVar' var space mVarId

instVar' var space mVarId
  	| Nothing	<- mVarId
	= freakout stage
	  	("instVar: can't instantiate var in space " % space
	  	% " var = " % show var)
		$ return Nothing
		
	| Just varId	<- mVarId
	= do
		-- increment the generator and write it back into the table.
		let varId'	= Var.incVarBind varId
		sVarGen		<##> Map.insert space varId'

		-- the new variable remembers what it's an instance of..
		let name	= pretty varId
		let var'	= (Var.new name)
			 { Var.nameSpace		= Var.nameSpace var
			 , Var.bind		= varId
			 , Var.info		= [Var.IParent var] }

		return $ Just var'


-- | Make a new variable in this namespace
newVarN :: NameSpace ->	SquidM Var
newVarN	space	
 = do
 	Just varId	<- liftM (Map.lookup space)
			$  gets stateVarGen
	
	let varId'	= Var.incVarBind varId
	sVarGen		<##> Map.insert space varId'
	
	let name	= pretty varId
	let var'	= (Var.new name)
			{ Var.nameSpace		= space 
			, Var.bind		= varId }
			
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



-- get/set projections
sTrace		= (stateTrace,		(\x s -> s { stateTrace		= x}))
sSigmaTable	= (stateSigmaTable,	(\x s -> s { stateSigmaTable	= x}))

sVarGen		= (stateVarGen,		(\x s -> s { stateVarGen	= x }))
sVarSub		= (stateVarSub,		(\x s -> s { stateVarSub	= x }))

sInst		= (stateInst,		(\x s -> s { stateInst		= x }))

sDataFields	= (stateDataFields,	(\x s -> s { stateDataFields	= x }))

