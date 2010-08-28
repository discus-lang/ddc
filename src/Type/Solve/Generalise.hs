
module Type.Solve.Generalise 
	(solveGeneralise)
where
import Type.Solve.BindGroup
import Type.Scheme
import Type.Extract
import Constraint.Exp
import Constraint.Bits
import Util
import Util.Graph.Deps
import DDC.Solve.Location
import DDC.Solve.State
import DDC.Type
import DDC.Var
import qualified Data.Set	as Set
import qualified Data.Map	as Map

debug	= True
trace s	= when debug $ traceM s

-- | Extract and generalise the binding group containing this variable, and return
--	the type scheme for this variable.
--
--	Only the type for the variable asked for is returned, but the whole binding
--	group is generalised and their schemes written back to the type graph.
--	
solveGeneralise :: TypeSource -> Var -> SquidM Type
solveGeneralise	src vGen
 = do
	trace	$ "\n"
		% "=============================================================\n"
		% "=== Generalise " % vGen % "\n"

	-- We now need to work out if this binding is a member of a mutually recursive group.
	--	If we're generalising one member, then constraints from all other members should
	--	already be in the graph. (sanity check this against genSusp)
	vsBindGroup	<- liftM Set.toList 
			$  liftM (fromMaybe Set.empty)
			$  bindGroup vGen

	-- Sort the bind group so that the variable that was originally asked for, vGen, is at
	--	the front of the list. We do this so its type is also at the front of the list
	--	returned from solveGeneralise_group
	let vsGroup_sorted = vGen : (vsBindGroup \\ [vGen])

	-- Generalise the group of bindings
	tsGen		<- solveGeneralise_group src vsGroup_sorted
	
	trace	$ "=== Generalise " % vGen % " done\n"
		% "=============================================================\n"

	-- Return the type that was requested by the caller
	let Just tGen	= takeHead tsGen
	
	return tGen


-- | Generalise a group of bindings.
--	All members of the binding group should be present in the list of suspended generalisations.

solveGeneralise_group
	:: TypeSource -> [Var]	-> SquidM [Type]
	
solveGeneralise_group src vsGen@(vGen : vsGenRest)
 = do
	genSusp		<- getsRef stateGenSusp
	trace	$ "    genSusp    = " % genSusp	% "\n"

	-- We first need to work out the types for all the free variables in the bind group.
	--	This can't be done statically before type inference starts because we wont know what
	--	names our projections will resolve to.
	
	-- 	However, when it comes time to generalise a group of bindings, all the projections that
	--	were their constraint branches will have been resolved to real function names and instantiated
	--	so we can work out the free variables of the bind group by using the branch containment 
	--	and instantiation maps in the solver state.
	
	-- First use the contains map to work out all the constraint branches contained
	--	within the ones that define the binding froup.
	gContains	<- getsRef stateContains

	-- start the search at the outermost branch for each of the bindings
	let bsRoots	= map Set.singleton
			$ map (\v -> BLet [v])
			$ vsGen

	-- work out all other branches contained within
	let bsContained	= Set.toList
			$ Set.unions 
			$ map (graphReachableS gContains)
			$ bsRoots

	-- this gives us all the variables that were bound within the code for this bind group.
	let vsBound	= catMap takeCBindVs bsContained
	
	-- Now collect up all the variables that were instantiated by the bind group
	gInstantiates	<- getsRef stateInstantiates
	let bsInst	= Set.toList
			$ Set.unions
			$ map (\b -> fromMaybe Set.empty $ Map.lookup b gInstantiates)
			$ bsContained
	
	let vsInst	= catMap takeCBindVs bsInst
	
	-- The free variables are the ones that were instantiated minus the ones that were bound.
	let vsEnv	= vsInst \\ vsBound
				
	trace	$ "    vsBound    = " % vsBound		% "\n"
		% "    vsInst     = " % vsInst		% "\n"
		% "    vsEnv      = " % vsEnv		% "\n"
	

	-- Extract the types for the free variables.
	--	This forms our type environment.
	--	As all the constraints for the branch to be generalised are in the graph now (including
	--	instantiations) all these types for the environment are guaranteed to be already
	--	generalised and in the graph

	Just tsEnv	<- liftM sequence
			$  mapM (extractType False) vsEnv

	-- Collect up the cids free in in the environment.
	--	These cids are fixed during the generalisation.
	let cidsEnv	= Set.unions $ map freeCids tsEnv
	trace	$ "    cidsEnv    = " % cidsEnv		% "\n"

	-- Extract and generalise the types for all of the members of the binding group
	tsScheme	<- mapM (solveGeneralise_single src cidsEnv) vsGen

	return tsScheme


-- | Extract a single type from the graph and generalise it using this set of fixed classIds.
--
solveGeneralise_single 
	:: TypeSource -> Set ClassId -> Var -> SquidM Type

solveGeneralise_single src cidsFixed vGen
 = do	
	-- Extract the type from the graph.
	Just tGraph	<- extractType False vGen

	-- Generalise the type into a scheme.
	tScheme		<- generaliseType vGen tGraph cidsFixed
	
	-- Add the scheme back to the graph.
	addSchemeToGraph src vGen tScheme

	-- Record that this type has been generalised, and delete the suspended generalisation
	stateGenDone `modifyRef` Set.insert vGen
	stateGenSusp `modifyRef` Set.delete vGen
	
	return tScheme


-- | Add a generalised type scheme to the graph
--	This is different from a Type.Feed.feedType because most of the type can be stored in a single
--	node in the graph instead of being distributed throughout.
--
addSchemeToGraph :: TypeSource -> Var -> Type -> SquidM ()
addSchemeToGraph src vGen tScheme
 = do
	-- call makeClass to get the classId of this var
	cidGen		<- ensureClassWithVar kValue src vGen 

	-- grab the class from the graph
	Just cls	<- lookupClass cidGen

	-- If this type has any FLets on it where the LHS is a (monomorphic) TClass 
	--	then this information is shared with the graph, and shouldn't be duplicated
	--	locally.
	let tScheme_stripped
		= stripFWheresT_mono tScheme

	case tScheme_stripped of 

	 -- If the scheme is just a classId we don't need to do anything
	 TVar _ UClass{}
		-> return ()

	 -- Update the class
	 -- TODO: Don't do this. Add the scheme somewhere else.
	 _	-> updateClass cidGen	
			cls { classUnified = Just $ NScheme tScheme_stripped }

