
module Type.Solve
	( squidSolve )

where

-----
import qualified Debug.Trace	as Debug

import qualified Data.Map	as Map
import qualified Util.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import Data.Array.IO

import Util
import Util.Graph.Deps

import System.IO

import Shared.Error
import qualified Shared.Var	as Var

import qualified Main.Arg	as Arg
import Main.Arg			(Arg)

import Constraint.Exp
import Constraint.Pretty
import Constraint.Bits

import Type.Exp
import Type.Pretty
import Type.Util
import Type.Error
import Type.Plate.Collect

import Type.State
import Type.Class
import Type.Scheme
import Type.Finalise
import Type.Feed
import Type.Trace
import Type.Context

import Type.Crush.Unify
import Type.Crush.Fetter
import Type.Crush.Shape
import Type.Crush.Proj
import Type.Crush.Effects
import Type.Crush.Sum

import Type.Check.CheckPure
import Type.Check.CheckConst

-----
debug	= True
trace s	= when debug $ traceM s
stage	= "Type.Solve"

-----
squidSolve 	
	:: [Arg]
	-> [CTree] 
	-> Map Var Var
	-> (Maybe Handle)
	-> IO SquidS

squidSolve 
	args 
	ctree 
	sigmaTable
	mTrace
 = do
	state1		<- squidSInit
 	let state2	= state1
			{ stateTrace		= mTrace
			, stateSigmaTable	= sigmaTable 
			, stateArgs		= Set.fromList args }
		
	state'		<- execStateT (solve args ctree)
			$ state2

	return state'
	   

-----
solve 	:: [Arg] 
	-> [CTree]
	-> SquidM ()

solve	args ctree	
 = do
	-- Slurp out the branch containment tree
	let treeContains	= Map.unions $ map slurpContains ctree
	modify (\s -> s { stateContains = treeContains })

	-- Feed all the constraints into the graph, generalising types when needed.
	solveCs ctree

	-- Do a final grind to make sure the graph is up to date
	solveCs [CGrind]
	
	-- Check if there were any errors
	errors	<- gets stateErrors
	
	if not $ isNil errors 
	 then do	
	 	trace	$ "\n=== solve: terminating with errors.\n"
	 		% "    errors = " % errors	% "\n"
			% "\n\n"

		return ()

	 else	solveFinalise
	 	
solveFinalise
 = do
	-- Generalise left over types.
	--	Types are only generalised before instantiations. If a function has been defined
	--	but not instantiated here (common for libraries) then we'll need to perform the 
	--	generalisation now so we can export its type scheme.
	--
	sGenSusp		<- gets stateGenSusp
	let sGeneraliseMe 	= Set.toList sGenSusp

	trace	$ "\n=== solve: Generalising left over types.\n"
		% "    sGeneraliseMe   = " % sGeneraliseMe % "\n"
	
	mapM_ solveGeneralise $ sGeneraliseMe
	

	-- When generalised schemes are added back to the graph we can end up with (var = ctor)
	--	constraints in class queues which need to be pushed into the graph by another grind.
	--
	solveCs [CGrind]
		
	return ()
 			
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
		feedConstraint c

		-- Record that this type is good to go.
		modify (\s -> s 
			{ stateGenDone	= Set.insert vDef (stateGenDone s) })

		solveNext cs

	-- type signature
	CSig src t1 t2
	 -> do	trace	$ "### CSig  " % padR 20 (pprStr t1) % " = " %> prettyTS t2 % "\n"

		t2_inst	<- instantiateT instVar t2
		feedConstraint (CSig src t1 t2_inst)

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
 	 -> do	trace	$ "### CEq  " % padR 20 (pprStr t1) % " = " %> prettyTS t2 % "\n"
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
	 -> do	trace	$ "### CProject " % j % " " % vInst % " " % tDict % " " % tBind
		feedConstraint c
		solveNext cs

	-- Projection dictionaries
	CDictProject src t@(TData v ts) vvs
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
		trace	$ "    path = " % path 	% "\n"

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
			$ [CEq src (TVar KData vUse) (TVar KData vInst)]
			++ cs 

	-- Instantiate a type from a let binding.
	--	It may or may not be generalised yet.
	CInstLet src vUse vInst
	 -> do	trace	$ "### CInstLet " % vUse % " " % vInst	% "\n"

	 	-- If the scheme is already generalised we can just extract it from the graph,
		--	otherwise we have to do the generalisation first.
		genDone		<- gets stateGenDone
		tScheme		<- if Set.member vInst genDone
					then do	Just tScheme_	<- extractType vInst
						return tScheme_

					else solveGeneralise vInst

	 	-- extract the scheme from the graph and instantiate it
		(tInst, tInstVs)<- instantiateT_table instVar tScheme

		-- Add information about how the scheme was instantiated
		sInst <##> Map.insert vUse
			(InstanceLet vInst vInst tInstVs tScheme)

		-- The type will be added via a new constraint
		solveNext
			$  [CEq src (TVar KData vUse) tInst]
			++ cs

	-- Instantiate a recursive let binding
	--	Once again, there's nothing to actually instantiate, but we record the 
	--	instance info so Desugar.ToCore knows how to translate the call.
	CInstLetRec src vUse vInst
	 -> do	trace	$ "### CInstLetRec " % vUse % " " % vInst % "\n"

	 	sInst <##> Map.insert vUse
	 		(InstanceLetRec vUse vInst Nothing)

		solveNext
			$ [CEq src (TVar KData vUse) (TVar KData vInst)]
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
	 		% "####################################\n"
	 		% "### Errors detected, bailing out\n\n"
			
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
		% "    path          = " % path 					% "\n"

	-- Look at our current path to see what branch we want to instantiate was defined.
	sGenDone		<- gets stateGenDone
	let bindInst 
		-- hmm, we're outside all branches
		| isNil path
		= BLet [vInst]

		-- var was imported, or already generalised.
		| Set.member vInst sGenDone
		= BLet [vInst]
		
		-- var was bound somewhere on our current path.
		| Just bind	<- find (\b -> elem vInst $ takeCBindVs b) path
		= case bind of
			BLambda _	-> BLambda [vInst]
			BDecon _	-> BDecon  [vInst]
			BLet _		-> BLet    [vInst]					
			BLetGroup _	-> BLet    [vInst]

	trace	$ "    bindInst      = " % bindInst					% "\n\n"
	
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

	solveCInst_simple cs c bindInst path sGenDone
	

-- These are the easy cases..

solveCInst_simple 
	cs 
	c@(CInst src vUse vInst)
	bindInst path sGenDone

	-- IF   the var has already been generalised/defined 
	-- THEN then we can extract it straight from the graph.
	| Set.member vInst sGenDone
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
	-- Load the info from the state that will help us work out
	--	what type to use for this binding.
	gContains	<- gets stateContains
	gInstantiates	<- gets stateInstantiates

	-- Restrict the instantiatesmap to just instantiations of let bindings.
	let gInstLet	= Map.map (Set.filter (\b -> b =@= BLet{})) gInstantiates

	-- The branch dependency graph is the union of the contains and instantiates graph
	let gDeps	=  Map.unionWith (Set.union) gContains gInstLet
	genSusp		<- gets stateGenSusp

{-	trace	$ "    gContains:\n" 	%> prettyBranchGraph gContains	% "\n\n"
		% "    gInstLet:\n" 	%> prettyBranchGraph gInstLet	% "\n\n"
		% "    gDeps:\n" 	%> prettyBranchGraph gDeps 	% "\n\n"
		% "    genSusp       = " % genSusp			% "\n\n"
-}
	solveCInst_find cs c bindInst path gDeps genSusp
	

solveCInst_find 
	cs 
	c@(CInst src vUse vInst)
	bindInst path gDeps genSusp
	
	-- If 	There is a suspended generalisation
	-- AND	we can reach the branch that we're in from the one we're trying to generalise
	-- THEN we're on a recursive loop and it's not safe to generalise.
 	| Set.member vInst genSusp
	, sDeps		<- graphReachableS gDeps (Set.singleton bindInst)
	, (p : _)	<- path
	, Set.member p sDeps
	= do 	
		trace	$ ppr "*   solveCInst_find: Recursive path\n"
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
--		trace	$ "=== Reorder.\n"
--			% "    queue =\n" %> (", " %!% map prettyCTreeS (c:cs)) % "\n\n"
	
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
	
--		trace	$ "    queue' =\n" %> (", " %!% map prettyCTreeS csReordered) % "\n\n"
	
		-- Carry on solving
		return	csReordered



-- | Extract and generalise the type scheme for this var.
--	The var should be present in the map of suspended generalisations

solveGeneralise ::	Var -> SquidM Type
solveGeneralise	vGen
 = do
	trace	$ "\n=============================================================\n"
		% "=== Generalise " % vGen % "\n"

	-- Work out the tyvars of all the bindings in this ones environment.
	--	We'll assume things are scoped properly, so the environment
	--	is just all the vars instantiated \\ all the vars bound.
	gContains	<- gets stateContains
	gInstantiated	<- gets stateInstantiates
	
	let bsBound	= Set.toList $ graphReachableS gContains (Set.singleton (BLet [vGen]))
	let vsBound	= catMap takeCBindVs bsBound
	
	-- all the vars instantiated by the contained branches
	let bsInst	= Set.toList
			$ Set.unions
			$ map (\b -> fromMaybe Set.empty $ Map.lookup b gInstantiated)
			$ bsBound
	
	let vsInst	= catMap takeCBindVs bsInst
	
	let vsEnv	= vsInst \\ vsBound
				
	trace	$ "    vsBound    = " % vsBound		% "\n"
		% "    vsInst     = " % vsInst		% "\n"
		% "    vsEnv      = " % vsEnv		% "\n"
	
	-- Extract the types present in the environment.
	--	No need to worry about generalisation here, if a var was in the environment
	--	then it was instantiated, so generalisation was already forced.
	Just tsEnv	<- liftM sequence
			$  mapM extractType vsEnv

	-- Collect up the cids free in the types in the environment.
	let cidsEnv	= nub $ catMap collectClassIds tsEnv

	trace	$ "    cidsEnv    = " % cidsEnv		% "\n"

	-- Extract the type from the graph.
	Just tGraph	<- extractType vGen

	-- Generalise the type into a scheme.
	tScheme		<- generaliseType vGen tGraph cidsEnv
	
	-- Add the scheme back to the graph.
	addSchemeToGraph vGen tScheme

	-- Record that this type has been generalised, and delete the suspended generalisation
	modify (\s -> s 
		{ stateGenDone	= Set.insert vGen (stateGenDone s) 
		, stateGenSusp	= Set.delete vGen (stateGenSusp s) })

	trace	$ "=== Generalise " % vGen % " done\n"
		% "=============================================================\n"
	
	return tScheme


-- | Add a generalised type scheme to the graph
--	This is different from a Type.Feed.feedType because most of the type can be stored in a single
--	node in the graph instead of being distributed throughout.
--
addSchemeToGraph
	:: Var -> Type -> SquidM ()
	
addSchemeToGraph vGen tScheme
 = do
	-- call makeClass to get the classId of this var
	cidGen		<- makeClassV TSNil KData vGen 

	-- grab the class from the graph
	Just cls	<- lookupClass cidGen

	-- If this type has any FLets on it where the LHS is a (monomorphic) TClass 
	--	then this information is shared with the graph, and shouldn't be duplicated
	--	locally.
	let (tScheme_stripped, fsMono) = stripMonoFLetsT tScheme

	case tScheme_stripped of 

	 -- If the scheme is just a classId we don't need to do anything
	 TClass{}	-> return ()

	 -- Update the class
	 _		-> updateClass cidGen	
				cls { classType = Just tScheme_stripped }
 	

	



-- | Work out which types are in the environmen of this branch
--	This makes use of the contains and instantiates maps from the state

traceEnvironment :: Var -> SquidM [Var]
traceEnvironment var
 = do	gContains	<- gets stateContains
 	gInstantiates	<- gets stateInstantiates

	-- Work out the names of the branches contained in this one
--	let branches	= graphReachableS gContains (Set.singleton [var])
	let branches	= []

	-- Collect the vars instantiated by all the 
	
	trace		$ "=== traceEnvironment " % var % "\n"
--			% "    branches = " % branches	% "\n"
	
	return []

	

prettyCTreeS :: CTree -> PrettyP
prettyCTreeS xx
 = case xx of
 	CBranch{} 
	 -> "\nBranch " 
	 	% branchBind xx 
			% " {" %> (", " %!% map prettyCTreeS (branchSub xx)) % "}"

--	CGen ts v env
--	 -> "(Gen " % v % ")"

	CLeave v
	 -> "(Leave " % v % ")"

	CInst ts vI vD
	 -> "(Inst " % vI % " " % vD % ")"

	_	  -> ppr "X"
	







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
pathLeave v
 = do	path	<- gets statePath
 	
	case path of 
	 (v' : vs)	
	   | v' == v
	   -> modify (\s -> s { statePath = vs })

	 _ -> panic stage
	 	$ "pathLeave: can't leave " % v % "\n"
		% "  path = " % path % "\n"
		
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

-- | Pretty print a branch graph	
-- prettyBranchGraph :: Map [Var] (Set [Var]) -> PrettyP
prettyBranchGraph graph
	= "\n" %!% ls
	where 	ls	= map (\(v, set)	
				-> (padR 16 $ pprStr v) % " -> " % set)
			$ Map.toList graph


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
 = do	
 	-- Don't unify if there are errors
 	errs		<- gets stateErrors
	if not $ isNil errs
	 then	return []
	 else do
	 	-- Run the unifier/projection resolver.
	 	-- Doing this can generate more constraints.
		trace	$ ppr "*   Grind.solveGrind, unifying.\n"
		csMore	<- solveUnify
		
		case csMore of
		 []	-> solveGrind_crush
		 _	-> return $ csMore ++ [CGrind]

solveGrind_crush
 = do	errs		<- gets stateErrors
 	if not $ isNil errs
	 then return []
	 else do
	  	-- Crush other things
	 	-- These don't generate more constraints, so once we've finished processing them the
		--	grind is complete.
	 
	 	-- Grab lists of interesting equivalence classes from the register.
		register		<- gets stateRegister

		let getReg bind		
			= return $ Set.toList $ (\(Just x) -> x) $ Map.lookup bind register

		regEReadH	<- getReg Var.EReadH
		regEReadT	<- getReg Var.EReadT
		regEWriteT	<- getReg Var.EWriteT

		regFLazyH	<- getReg Var.FLazyH
		regFMutableT	<- getReg Var.FMutableT
		regFConstT	<- getReg Var.FConstT
 
	 	trace	$ "*   Grind.solveGrind: crushing.\n"
			% "    regEReadT    = " % regEReadT	% "\n"
			% "    regEReadH    = " % regEReadH	% "\n"
			% "    regFLazyH    = " % regFLazyH	% "\n"
			% "    regFMutableT = " % regFMutableT	% "\n"
			% "    regFConstT   = " % regFConstT	% "\n"
			% "\n\n"


		-- Now that the graph is unified, we can try and crush out some of the simpler compound
		--	effects and fetters. Crushing these constructors will not add any more constraints
		--	to nodes in the graph, so there is no need to interleave it with unification.

		-- Crush out EReadTs
		trace	$ ppr "*   Grind.solveGrind, crushing EReadHs, EReadTs, EWriteTs\n"
		mapM_ crushEffectC (regEReadH ++ regEReadT ++ regEWriteT)

		-- Crush out FLazyHs, FMutableTs
		trace	$ ppr "*   Grind.solveGrind, crushing FLazyHs, FMutableTs\n"
		mapM_ crushFetterC (regFLazyH ++ regFMutableT ++ regFConstT)
	
		-- all done
		trace	$ "\n"
			% "=== Grind.solveGrind done\n"
			% "=============================================================\n"
			% "\n\n"

		return 	[]


-- Unify some classes in the graph.
--	The crushing of Shape fetters is interleaved with batches of unification
--	because the crushing can add more constraints to the graph.
--
solveUnify 
	:: SquidM [CTree]

solveUnify 	
 = do	-- get the list of nodes which have constraints waiting to be unified.
	-- This clears the active list in the graph, so we must make sure
	--	to handle all of these before returning.
	
 	queued		<- liftM Set.toList $ clearActive 		

	-- get classes waiting to be projected
	regProj		<- getRegProj		
	regShape	<- getRegShapes

	-- check if there are any errors in the state
	errors		<- gets stateErrors

	trace	$ "*   Grid.solveUnify\n"
		% "    queued      = " % queued			% "\n"
		% "    regProj     = " % regProj		% "\n"
		% "    regShape    = " % regShape		% "\n"
		% "    errors:\n     " %> "\n" %!% errors	% "\n"

	solveUnifySpin queued regProj regShape errors

solveUnifySpin queued regProj regShape errors

	-- If there are errors in the solver state then bail out.
	| not $ isNil errors
	= return []
	
	-- If no nodes need to be unified and there are no projections or
	--	shape constraints left in the graph then we're done.
	| []	<- queued
	, []	<- regProj
	, []	<- regShape
	= return []
	
	-- Otherwise, try to unify or crush something.
	| otherwise
	= solveUnifyWork queued regProj regShape errors

solveUnifyWork queued regProj regShape errors
 = do	
  	-- Unify the queued classes.
  	mapM_ crushUnifyClass queued

	-- Try to crush out some of the Shape fetters.
	progressShape	<- liftM or 
			$  mapM crushShape regShape

	-- Try to crush out some of the Projection fetters.
	newQs		<- liftM (concat . catMaybes)
			$  mapM crushProjClassT regProj
	
	let progressProj = not $ isNil newQs

	-- decide what to do now..
	let res
	  
		-- If we've crushed some projections then bail out and return the new constraints
		| progressProj
		= return newQs

		-- If the queue is empty and we couldn't make progress on either projections
		--	or shape constraints then we're stuck and its time to bail out.
		--
		| isNil queued  
		,   (isNil regProj   && not progressShape)
		 || (isNil regShape  && not progressProj)
		 || (not progressShape  && not progressProj)

		= do
			trace	$ ppr "*   Grind.solveUnify: no progress\n"
			return newQs

		-- Keep going..
		| otherwise
		= solveUnify

	res


-- | Get the list of classe which contain projection fetters.
getRegProj :: SquidM [ClassId]			  	
getRegProj
 = do	register	<- gets stateRegister
	let regProj
		= Set.toList
		$ (\(Just x) -> x)
		$ Map.lookup Var.FProj register

	return regProj


-- | Get the list of classes which contain shape fetters.
getRegShapes :: SquidM [ClassId]
getRegShapes
 = do	register	<- gets stateRegister
	let regShapes
		= Set.toList
		$ (\(Just x) -> x)
		$ Map.lookup (Var.FShape 0) register

	return regShapes


	
