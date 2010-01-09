{-# OPTIONS -fwarn-unused-imports -fno-warn-incomplete-record-updates #-}

module Type.Solve
	( squidSolve )

where

-----

import qualified Main.Arg	as Arg
import Main.Arg			(Arg)

import Constraint.Bits
import Constraint.Exp

import Type.Check.Main
import Type.Check.Instances
import Type.Check.SchemeDanger

import Type.Crush.Unify
import Type.Crush.Fetter
import Type.Crush.Shape
import Type.Crush.Proj
import Type.Crush.Effects

import Type.Extract
import Type.State
import Type.Class
import Type.Scheme
import Type.Feed
import Type.Location
import Type.Pretty
import Type.Util
import Type.Plate.Collect
import Type.Exp


import Shared.Error
import qualified Shared.Var	as Var
import qualified Shared.VarBind	as Var

import Util
import Util.Graph.Deps

import qualified Util.Data.Map	as Map
import Util.Data.Map		(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import System.IO


-----
-- debug the solver
debug	= True
trace s	= when debug $ traceM s

-- debug the grinder
debugG	= False
traceG s = when debugG $ traceM s

stage	= "Type.Solve"

-- Solve some type constraints.
squidSolve 	
	:: [Arg]		-- ^ Compiler args.
	-> [CTree] 		-- ^ The type constraints to solve.
	-> Map Var Var		-- ^ table of value vars to type evars.
	-> Set Var		-- ^ type vars of value vars which are bound at top level.
	-> Maybe Handle		-- ^ If Just, write solve trace to this handle.
	-> Bool			-- ^ Whether to require main fns defined here to have the main type.
	-> IO SquidS

squidSolve 
	args ctree sigmaTable 
	vsBoundTopLevel mTrace blessMain
 = do
	state1		<- squidSInit
 	let state2	= state1
			{ stateTrace		= mTrace
			, stateSigmaTable	= sigmaTable 
			, stateVsBoundTopLevel	= vsBoundTopLevel
			, stateArgs		= Set.fromList args }
		
	state'		<- execStateT (solve args ctree blessMain)
			$ state2

	return state'
	   

-----
solve 	:: [Arg] 
	-> [CTree]
	-> Bool		-- ^ whether to require any 'main' functions defined in this module
			--	to have the main type.
	-> SquidM ()

solve	args ctree blessMain	
 = do
	-- Slurp out the branch containment tree
	let treeContains	= Map.unions $ map slurpContains ctree
	modify (\s -> s { stateContains = treeContains })

	-- Feed all the constraints into the graph, generalising types when needed.
	solveCs ctree

	-- Do a final grind to make sure the graph is up to date
	solveCs [CGrind]
	
	trace $ ppr "\n=== solve: post solve checks.\n"
	
	-- If the main function was defined, then check it has an appropriate type.
	errors_checkMain	<- gets stateErrors
	when (null errors_checkMain && blessMain)
		checkMain

	-- Check there is an instance for each type class constraint left in the graph.
	errors_checkInstances	<- gets stateErrors
	when (null errors_checkInstances)
		checkInstances

	errors_checkSchemes	<- gets stateErrors
	when (null errors_checkSchemes)
		checkSchemes

	-- Report how large the graph was
	graph		<- gets stateGraph
	trace	$ "=== Final graph size: " % graphClassIdGen graph % "\n"

	-- Report whether there were any errors
	errors	<- gets stateErrors
	trace   $ "=== Errors: " % errors % "\n"
 			
-----
solveCs :: [CTree] 
	-> SquidM Bool

solveCs []	
 = 	return False

solveCs	(c:cs)
 = case c of

	-- def
	CDef src t1@(TVar k vDef) t
 	 -> do	
--	 	trace	$ "### Def  " % vDef %> ("\n:: " % prettyTypeSplit t) % "\n\n"
--		feedConstraint c

		-- Record the constraint in the solver state
		modify $ \s -> s 
			{ stateDefs	= Map.insert vDef t (stateDefs s) }

		solveNext cs


	-- Strip fetters off the sig before adding it to the graph
	--	We only really need sigs for guiding projection resolution.
	--	We also need to avoid the wormhole problem for closures:
	--
	-- 	f1' :: a -> b -($c3)> c
	--          :- $c3 = x : b
	--
	--	f1  :: a -($c1)> b -($c2)> c
	--	    :- $c1 = $c2 \ z
	--	    ,  $c2 = z : b
	--
	-- If f1' is the sig and f1 the type derived from the constraints,
	--	unifying the two types gives
	--
	--	    :: a -($c1)> b -($c2)> c
	--	    :- $c1 = { x : b; }
	--	    ,  $c2 = { x : b; z : b }
	--
	-- Which is wrong. If we strip fetters from the sig before adding
	-- it to the graph we get the desired shape information and avoid 
	-- this problem.
	--
	-- type signature
	CSig src t1 t2
	 -> do	trace	$ "### CSig  " % t1 % "\n"
	 		% "    t2:\n" %> prettyTS t2 % "\n\n"

		t2_inst	<- instantiateT instVar t2
		let (t2_strip, _) 
			= stripFWheresT t2_inst

		trace	$ "    t2_strip:\n" %> prettyTS t2_strip % "\n\n"

		feedConstraint (CSig src t1 t2_strip)

		solveNext cs


	-----
	CBranch{}
	 -> do	traceIE
	 	trace	$ "\n### Branch" % "\n"

		-- record that we've entered this branch
		--	Don't add BGroups to the path, they help with working out the environmet only.
		let bind	= branchBind c
		pathEnter bind
			
		solveNext (branchSub c ++ [CLeave bind] ++ cs)



	-- Equality Constraint
	CEq src t1 t2
 	 -> do	trace	$ "### CEq  " % padL 20 t1 % " = " %> prettyTS t2 % "\n"
		feedConstraint c
		solveNext cs
	
	CEqs src ts
	 -> do	trace	$ "### CEqs " % ts % "\n"
 	 	feedConstraint c
		solveNext cs

	CClass src v ts
	 -> do	trace	$ "### CClass " % v % " " % ts % "\n"
	 	feedConstraint c
		solveNext cs

	-- data fields
	CDataFields src v vs fs
	 -> do	trace	$ "### DataFields " % v % " " % vs % "\n"
		sDataFields	<##> Map.insert v (vs, fs)
		solveNext cs

	-- Projection constraints
	CProject src j vInst tDict tBind
	 -> do	trace	$ "### CProject " % j % " " % vInst % " " % tDict % " " % tBind	% "\n"
		feedConstraint c
		solveNext cs

	-- Projection dictionaries
	CDictProject src t vvs
	 | Just (v, _, ts)	<- takeTData t
	 -> do	trace	$ "### CDictProj " % t % "\n"
	 	modify $ \s -> s { stateProject
	 				= Map.insert v (t, vvs) (stateProject s)}
		solveNext cs

	-- Generalisation
	CGen src t1@(TVar k v1)
	 -> do	trace	$ "### CGen  " % prettyTS t1 %  "\n"
	 	modify (\s -> s { stateGenSusp
					= Set.insert v1 (stateGenSusp s) })
		solveNext cs

	-- Instantiation
	CInst{}	
	 -> do	cs'	<- solveCInst cs c
	 	solveNext cs'

	-- Type class instance
	CClassInst src v ts
	 -> do	trace	$ "### CClassInst " % v % " " % ts % "\n"
	 
	 	-- stash this in the map of instances
	 	--	We'll use this later to discharge class constraints in type schemes during 
		--	the generalisation process.
	 	modify $ \s -> s { 
			stateClassInst = Map.alter
				(\mis -> case mis of
					Nothing	-> Just [FConstraint v ts]
					Just is	-> Just (FConstraint v ts : is)) 
				v (stateClassInst s) }

		solveNext cs
	
	-- Internal constraints ----------------------------------------------------------------------	
	--	These are ones we've left for ourselves.

	-- This tells us that we've processed all the constraints from this branch.
	--	Pop the bound vars from this branch off the path.
	CLeave vs
	 -> do	trace	$ "\n### CLeave " % vs % "\n"
	 	path	<- gets statePath
--		trace	$ "    path = " % path 	% "\n"

		-- We're leaving the branch, so pop ourselves off the path.
	 	pathLeave vs
		traceIL

		solveNext cs	 

	-- Do a graph grind
	CGrind	
	 -> do	csMore	<- solveGrind
	 	solveNext (csMore ++ cs)

	-- Instantiate a type from a lambda binding
	--	There's nothing to really instantiate, just set the use type to the def type.
	CInstLambda src vUse vInst
	 -> do	trace	$ "### CInstLambda " % vUse % " " % vInst % "\n"

	 	sInst <##> Map.insert vUse
			(InstanceLambda vUse vInst Nothing)

		solveNext
			$ [CEq src (TVar kValue vUse) (TVar kValue vInst)]
			++ cs 


	-- Instantiate a type from a let binding.
	--	It may or may not be generalised yet.
	CInstLet src vUse vInst
	 -> do	trace	$ "### CInstLet " % vUse % " " % vInst	% "\n"

		defs		<- gets stateDefs
		genDone		<- gets stateGenDone

		let getScheme
			-- The scheme is in our table of external definitions
			| Just tt	<- Map.lookup vInst defs
			= return (Just tt)

			-- The scheme has already been generalised so we can extract it straight from the graph			
			| Set.member vInst genDone
			= do	Just cid	<- lookupVarToClassId vInst

				-- we need to make sure that no new mutability constraints have
				--	crept in and made any more vars in this type dangerous since
				--	we generalised it.
				errs		<- checkSchemeDangerCid cid
				case errs of
				 []	-> do
				 	Just t	<- extractType False vInst
					return	$ Just t
				 _	
				  -> do
					addErrors errs
				 	return Nothing
				
			-- Scheme hasn't been generalised yet
			| otherwise
			= do	t	<- solveGeneralise (TSI $ SIGenInst vUse) vInst
				return $ Just t
			
		mScheme	<- getScheme
		case mScheme of
		 Just tScheme
		  -> do		
		 	-- Instantiate the scheme
			(tInst, tInstVs)<- instantiateT_table instVar tScheme

			-- Add information about how the scheme was instantiated
			sInst <##> Map.insert vUse
				(InstanceLet vInst vInst tInstVs tScheme)

			-- The type will be added via a new constraint
			solveNext
				$  [CEq src (TVar kValue vUse) tInst]
				++ cs
		 Nothing
		  ->	return True


	-- Instantiate a recursive let binding
	--	Once again, there's nothing to actually instantiate, but we record the 
	--	instance info so Desugar.ToCore knows how to translate the call.
	CInstLetRec src vUse vInst
	 -> do	trace	$ "### CInstLetRec " % vUse % " " % vInst % "\n"

	 	sInst <##> Map.insert vUse
	 		(InstanceLetRec vUse vInst Nothing)

		solveNext
			$ [CEq src (TVar kValue vUse) (TVar kValue vInst)]
			++ cs

	-- Some other constraint	
	_ -> do
	 	trace $ "--- Ignoring constraint " % c % "\n"
		solveNext cs
	 	
solveNext cs
 = do 	err	<- gets stateErrors
	if isNil err
	 then 	solveCs cs
	 else do
	 	trace	$ "\n"
	 		% "#####################################################\n"
	 		% "### solveNext: Errors detected, bailing out\n\n"
			
		return True



-- Handle a CInst constraint
--	We may or may not be able to actually instantiate the desired type right now.
--
--	There may be projections waiting to be resolved which require us to reorder
--	constraints, generalise and instantiate other types first.
--
--	This function diagnoses where we're at, 
--		and creates CInstGeneralise and CInstExtract which trigger the 
--		real instantiation.

solveCInst :: [CTree] -> CTree -> SquidM [CTree]
solveCInst 	cs c@(CInst src vUse vInst)
 = do
	path			<- gets statePath
	trace	$ "\n"
		% "### CInst " % vUse % " <- " % vInst					% "\n"
--		% "    path          = " % path 					% "\n"

	-- Look at our current path to see what branch we want to instantiate was defined.
	sGenDone	<- gets stateGenDone
	sDefs		<- gets stateDefs
	let bindInst 
		-- hmm, we're outside all branches
		| isNil path
		= BLet [vInst]

		-- var was imported from another module
		| Map.member vInst sDefs
		= BLet [vInst]

		-- var has already been generalised
		| Set.member vInst sGenDone
		= BLet [vInst]
		
		-- var was bound somewhere on our current path.
		| Just bind	<- find (\b -> elem vInst $ takeCBindVs b) path
		= case bind of
			BLambda _	-> BLambda [vInst]
			BDecon _	-> BDecon  [vInst]
			BLet _		-> BLet    [vInst]					
			BLetGroup _	-> BLet    [vInst]

--	trace	$ "    bindInst      = " % bindInst					% "\n\n"
	
	-- Record the current branch depends on the one being instantiated
	-- 	Only record instances of Let bound vars, cause these are the ones we care
	--	about when doing the mutual-recusion check.
	--	For a Projection we'll add this after we work out what vInst should be
	case path of

	 -- We might instantiate some projection functions during solveGrind, after leaving 
	 -- 	all constraint branches, and with the path empty.
	 []	-> return ()
	 (p:_)	-> graphInstantiatesAdd p bindInst

	sGenDone	<- gets stateGenDone
	sDefs		<- gets stateDefs

	solveCInst_simple cs c bindInst path sGenDone sDefs
	

-- These are the easy cases..

solveCInst_simple 
	cs 
	c@(CInst src vUse vInst)
	bindInst path sGenDone sDefs

	-- IF   the var has already been generalised/defined 
	-- THEN then we can extract it straight from the graph.
	|   Set.member vInst sGenDone
	 || Map.member vInst sDefs
	= do	
		trace	$ ppr "*   solveCInst_simple: Scheme is in graph.\n"
		return	$ CGrind : (CInstLet src vUse vInst) : cs

	-- If	The var we're trying to instantiate is on our path
	-- THEN	we're inside this branch.
	| (bind : _)	<- filter (\b -> (not $ b =@= BLetGroup{})
				      && (elem vInst $ takeCBindVs b)) path
	= do	
		trace	$ ppr "*   solceCInst_simple: Inside this branch\n"

		-- check how this var was bound and build the appropriate InstanceInfo
		--	the toCore pass will use this to add the required type params
		--	to this call.
		case bind of
			BLet{}		-> return $ (CInstLetRec src vUse vInst) : cs
			BLambda{}	-> return $ (CInstLambda src vUse vInst) : cs
			BDecon{}	-> return $ (CInstLambda src vUse vInst) : cs

	| otherwise
	= solveCInst_let cs c bindInst path
	

-- If we're not inside the branch defining it, it must have been defined 
--	somewhere at this level. Build dependency graph so we can work out if we're on a recursive loop.

solveCInst_let 
	cs 
	c@(CInst src vUse vInst)
	bindInst path
 = do
	genSusp		<- gets stateGenSusp
	trace	$ "    genSusp       = " % genSusp			% "\n\n"

	-- Work out the bindings in this ones group
	mBindGroup	<- bindGroup vInst

	solveCInst_find cs c bindInst path mBindGroup genSusp
	

solveCInst_find 
	cs 
	c@(CInst src vUse vInst)
	bindInst path mBindGroup genSusp
	
	-- If the binding to be instantiated is part of a recursive group and we're not ready
	--	to generalise all the members yet, then we must be inside one of them.
	--	
	| Just vsGroup	<- mBindGroup
	, not $ and $ map (\v -> Set.member v genSusp) $ Set.toList vsGroup
	= do 	
		trace	$ ppr "*   solveCInst_find: Recursive binding.\n"
		return	$ (CInstLetRec src vUse vInst) : cs



		
	-- IF	There is a suspended generalisation
	-- AND	it's not recursive
	-- THEN	generalise it and use that scheme for the instantiation
	| Set.member vInst genSusp
	= do	
		trace	$ ppr "*   solveCInst_find: Generalisation\n"

		return	$ CGrind : (CInstLet src vUse vInst) : cs
			
	-- The type we're trying to generalise is nowhere to be found. The branch for it
	--	might be later on in the constraint list, but we need it now.
	-- 	Reorder the constraints to process that branch first before
	--	we try the instantiation again.
	| otherwise
	= do	
		trace	$ ppr "=== Reorder.\n"
--			% "    queue =\n" %> (", " %!% map ppr (c:cs)) % "\n\n"
	
		let floatBranch prev cc
			= case cc of
				(c@(CBranch { branchBind = BLet [vT] }) : cs)
				 | vT == vInst
				 -> c : (prev ++ cs)
				 
				(c : cs) 
				 -> floatBranch (prev ++ [c]) cs
				 
				[] -> panic stage
				 	$ "floatBranch: can't find branch for " % vInst % "\n"
					
		-- Reorder the constraints so the required branch is at the front
		let csReordered	= floatBranch [] (c:cs)
	
--		trace	$ "    queue' =\n" %> (", " %!% map ppr csReordered) % "\n\n"
	
		-- Carry on solving
		return	csReordered



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
	genSusp		<- gets stateGenSusp
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
	gContains	<- gets stateContains

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
	gInstantiates	<- gets stateInstantiates
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
	let cidsEnv	= Set.fromList $ catMap collectClassIds tsEnv
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
	modify (\s -> s 
		{ stateGenDone	= Set.insert vGen (stateGenDone s) 
		, stateGenSusp	= Set.delete vGen (stateGenSusp s) })

	return tScheme


-- Use the current contains and instantiates maps in the solver state to work out what
--	bindings are in this one's bind group. ie, what set of mutually recursive bindings this
--	one belongs to.
--
--	This should only be called when generalising this binding, because it uses the current
--	path to work out what other variables were let bound at the same scope.
--
--	returns 	Nothing		if the binding is not recursive
--	otherwise	Just vs		where vs are the vars of the let bindings which are
--					recursive with this one
--
bindGroup 
	:: Var
	-> SquidM (Maybe (Set Var))
	
bindGroup vBind
 = {-# SCC "bindGroup" #-} bindGroup' vBind

bindGroup' vBind
 = do
	trace	$ "*   bindGroup " % vBind % "\n"
	let bBind	= BLet [vBind]

	-- Grab what path we're on and follow it back up until we find the CBind that tells
	--	us what other variables were let-bound at the same level as this one.
	path		<- gets statePath
--	trace	$ "    path          = " % path	% "\n"

	let mbUseGroup	= find	(\b -> case b of
					BLetGroup vs		-> elem vBind vs
					_			-> False)
					path

	-- if there is no let group on the path this means the binding was defined 
	--	but never instantiated..
	let bUseGroup@(BLetGroup vsLetGroup)
			= case mbUseGroup of
				Nothing	-> BLetGroup []
				Just bb	-> bb

--	trace	$ "    bUseGroup     = " % bUseGroup			% "\n\n"

	-- Build the constraint dependency graph.
	--	This is a union of the contains and instantiates map.

	-- Lookup the branch containment graph and prune it down to just those branches
	--	reachable from the let group containing the binding to generalise.
	gContains_	<- gets stateContains
	let gContains	= graphPrune gContains_ bUseGroup
	let sContains	= Set.unions (Map.keysSet gContains : Map.elems gContains)

	-- Lookup the branch instantiation graph and prune it down to just those members
	--	that are present in the containment graph. 
	--	Also restrict it to just instantiations of let bindings. 
	gInst1		<- gets stateInstantiates
	let gInst2	= Map.filterWithKey (\k a -> Set.member k sContains) gInst1
	let gInst	= Map.map (Set.filter (\b -> b =@= BLet{})) gInst2

	-- The dependency graph is the union of both of these.
	let gDeps	=  Map.unionWith (Set.union) gContains gInst

--	trace	$ "    gContains:\n" 		%> ppr gContains	% "\n\n"
--		% "    gInst:\n"		%> ppr gInst		% "\n\n"		
--		% "    gDeps:\n"		%> ppr gDeps		% "\n\n"
	-- Work out all the stronly connected components (mutually recursive groups) 
	--	reachable from the binding which needs its type generalised.
	let sccs_all	= graphSCC gDeps bBind
--	trace	$ "    sccs_all    = " % sccs_all	% "\n"
	
	-- Only keep the sccs that this binding is actually a member of.
	--	We need to this otherwise we'll also pick up mutually recursive groups defined 
	--	at the same level, but which vBind isn't actually a member of.
	let sccs_member	= filter (Set.member bBind) sccs_all
-- 	trace	$ "    sccs_member = " % sccs_member	% "\n"

	-- Pack all the individual sccs together, they're in the same binding group.
	let scc		= Set.unions sccs_member

	-- The containment graph contains information about lambda bindings as well.
	--	They're in the scc, but we only care about let bindings here.
	let scc_let
		= Set.fromList
		$ catMaybes
		$ map	(\b -> case b of
				BLet [v] 
				 | elem v vsLetGroup	-> Just v
				_	 		-> Nothing)
		$ Set.toList scc

--	trace	$ "    scc_let     = " % scc_let	% "\n\n"

	if Set.null scc_let 
		then return Nothing
		else return $ Just scc_let
		


-- | Add a generalised type scheme to the graph
--	This is different from a Type.Feed.feedType because most of the type can be stored in a single
--	node in the graph instead of being distributed throughout.
--
addSchemeToGraph
	:: TypeSource -> Var -> Type -> SquidM ()
	
addSchemeToGraph src vGen tScheme
 = do
	-- call makeClass to get the classId of this var
	cidGen		<- makeClassV src kValue vGen 

	-- grab the class from the graph
	Just cls	<- lookupClass cidGen

	-- If this type has any FLets on it where the LHS is a (monomorphic) TClass 
	--	then this information is shared with the graph, and shouldn't be duplicated
	--	locally.
	let (tScheme_stripped, _) = stripMonoFWheresT tScheme

	case tScheme_stripped of 

	 -- If the scheme is just a classId we don't need to do anything
	 TClass{}	-> return ()

	 -- Update the class
	 _		-> updateClass cidGen	
				cls { classType = Just tScheme_stripped }


-- | Push a new var on the path queue.
--	This records the fact that we've entered a branch.

pathEnter :: CBind -> SquidM ()
pathEnter BNil	= return ()
pathEnter v
 = modify (\s -> s { statePath = v : statePath s })


-- | Pop a var off the path queue
--	This records the fact that we've left the branch.

pathLeave :: CBind -> SquidM ()

-- BNil's don't get pushed onto the path
pathLeave BNil	= return ()

pathLeave bind
 = do	path	<- gets statePath
 	
	let res	
		-- When leaving a LetGroup, generalise any types that haven't already
		--	been generalised. Especially in library code, we're not expecting to see instantiations 
		--	of top level variables in this module - but we still need the types for the interface file.
		| [BLetGroup vs]	<- path
		, bind == BLetGroup vs
		= do	solveFinalise
			modify $ \s -> s { statePath = [] }

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
	

solveFinalise
 = do
	-- Generalise left over types.
	--	Types are only generalised before instantiations. If a function has been defined
	--	but not instantiated here (common for libraries) then we'll need to perform the 
	--	generalisation now so we can export its type scheme.
	--
	sGenSusp		<- gets stateGenSusp
	let sGenLeftover 	= Set.toList sGenSusp

	trace	$ "\n== Finalise ====================================================================\n"
		% "     sGenLeftover   = " % sGenLeftover % "\n"
	
	solveCs [CGrind]

	-- If grind adds errors to the state then don't do the generalisations.
	errs	<- gotErrors
	when (not errs)
	 $ do 	
	 	mapM_ (solveGeneralise (TSI $ SIGenFinal)) $ sGenLeftover

		-- When generalised schemes are added back to the graph we can end up with (var = ctor)
		--	constraints in class queues which need to be pushed into the graph by another grind.
		--
		solveCs [CGrind]
		return ()
	
		
	return ()


		
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


-- | Perform unification, resolve projections and grind out any available effects 
--	or fetters in the graph.
--
--	solveGrind may not be able to completely grind out all constraints because
--	the crushing of projections may require the projection function to be instantiated, 
--	triggering generalisation and requiring another grind.
--
--	If solveGrind returns no constraints, the grind succeeded and no crushable constructors
--	remain in the graph.
--
--	If solveGrind returns constraints, then they need to be processed before continuing the grind.
--	In this case the last constraint in the list will be another CGrind.
--
solveGrind 
	:: SquidM [CTree]

solveGrind
 = do	-- if there are already errors in the state then don't start the grind.
 	errs		<- gets stateErrors
	if isNil errs
	 then do 
		trace	$ ppr "-- Grind Start --------------------------------\n"
	 	cs	<- solveGrindStep
		trace	$ ppr "------------------------------------ Grind Stop\n"

		return cs

	 else return []
		
			
solveGrindStep 
 = do	traceG	$ "\n"
 		% "*   solveGrindStep\n"

	 -- get the set of active classes
 	active	<- liftM Set.toList $ clearActive
	
	traceG	$  "    active classes:\n"
		%> active	%  "\n"

	-- make sure all classes are unified
	progressUnify
		<- liftM or
		$  mapM crushUnifyClass active


	errors	<- gets stateErrors
	when (length errors > 0) $ do
		liftIO $ exitWithUserError [] errors

	-- grind those classes
	(progressCrush, qssMore)
		<- liftM unzip
		$  mapM grindClass active
 	
	-- split classes into the ones that made progress and the ones that
	--	didn't. This is just for the trace stmt below.
	let activeProgress	   = zip active progressCrush
	let classesWithProgress	   = [cid | (cid, True)  <- activeProgress]
	let classesWithoutProgress = [cid | (cid, False) <- activeProgress]
	
	traceG	$ "*    classes that made progress:\n" 
		%> classesWithProgress % "\n"

	 	% "     classes without   progress:\n" 
		%> classesWithoutProgress % "\n"

	let qsMore	= concat qssMore
	errs		<- gets stateErrors
		
	traceG	$ ppr "\n"
	let next
		-- if we've hit any errors then bail out now
		| not $ null errs
		= return []

		-- if we've crushed a projection and ended up with more constraints
		--	then stop grinding for now, but ask to be resumed later when
		--	new constraints are added.
		| not $ null qsMore
		= return (qsMore ++ [CGrind])
		
		-- if we've made progress then keep going.
		| progressUnify || or progressCrush
		= solveGrindStep 
		
		-- no progress, all done.
		| otherwise
		= return []
		
	next


grindClass :: ClassId -> SquidM (Bool, [CTree])
grindClass cid
 = do	Just c	<- lookupClass cid
	grindClass2 cid c

grindClass2 cid c@(ClassForward cid')
	= grindClass cid'
	
grindClass2 cid c@(ClassNil)
	= return (False, [])
	 	
-- type nodes
grindClass2 cid c@(Class	
			{ classType 	= mType
			, classKind	= k 
			, classFetters	= fs})
 = do	
	-- if a class contains an effect it might need to be crushed
--	traceG	$ ppr "   - crushing effects\n"
	progressCrushE	
		<- case k of
			kE | kE == kEffect	-> crushEffectC cid
			_			-> return False

	-- try and crush other fetters in this class
--	traceG	$ ppr "   - crushing type fetters\n"
	progressCrush
		<- case fs of
			[]	-> return False
			_	-> crushFetterC cid
	
	return	( progressCrushE || progressCrush
		, [])
	
-- fetter nodes
grindClass2 cid c@(ClassFetter { classFetter = f })
 = do
	-- crush projection fetters
--	traceG	$ ppr "   - crushing projections\n"
	qsMore	<- case f of
			FProj{}	-> crushProjClassT cid
			_	-> return Nothing
			
	let progressProj
		= isJust qsMore
		
	-- crush shape fetters
--	traceG	$ ppr "   - crushing shapes\n"
	let isFShape b
		= case b of
			Var.FShape _	-> True
			_		-> False

	progressShape
		<- case f of
			FConstraint v _
			 | isFShape $ Var.bind v	-> crushShape cid
			_				-> return False
		
	-- crush other fetters
--	traceG	$ ppr "   - crushing other fetters\n"
	progressCrush
		<- crushFetterC cid
		
	return	( progressProj || progressShape || progressCrush
		, fromMaybe [] qsMore )
		

