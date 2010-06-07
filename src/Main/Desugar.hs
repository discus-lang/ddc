module Main.Desugar
	( desugarInferKinds
	, desugarElaborate
	, desugarProjectEta
	, desugarProject
	, desugarSlurpConstraints
	, desugarSolveConstraints
	, desugarToCore )
where
import Main.Dump
import DDC.Main.Pretty
import DDC.Main.Arg
import DDC.Main.Error
import DDC.Base.SourcePos
import DDC.Var
import Data.IORef
import System.IO			(hFlush)
import Util				hiding (null, elem)
import qualified DDC.Solve.InstanceInfo	as T
import qualified DDC.Type		as T
import qualified Data.Foldable		as Foldable
import qualified Constraint.Simplify	as N
import qualified Constraint.Exp		as N
import qualified Core.Exp		as C
import qualified Type.Export		as T
import qualified Type.Dump		as T
import qualified Type.Solve		as T
import qualified Type.State		as T
import qualified Desugar.ToCore		as D
import qualified Desugar.Slurp.State	as D
import qualified Desugar.Slurp.Slurp	as D
import qualified Desugar.ProjectEta	as D
import qualified Desugar.Project	as D
import qualified Desugar.Kind		as D
import qualified Desugar.Elaborate	as D
import qualified Desugar.Plate.Trans	as D
import qualified Desugar.Exp		as D
import qualified Data.Map		as Map
import qualified Data.Set		as Set


-- InferKinds --------------------------------------------------------------------------------------
desugarInferKinds
	:: (?args :: [Arg]			-- command line args
	 ,  ?pathSourceBase :: FilePath)	-- base path to source file
	=> String				-- unqiue name
	-> D.Tree SourcePos			-- header tree
	-> D.Tree SourcePos			-- source tree

	-> IO	( D.Tree SourcePos		-- new header tree
		, D.Tree SourcePos		-- new source tree
		, Map Var T.Kind)		-- kind of every type constructor
	
desugarInferKinds 
	unique
	treeHeader treeSource
	
 = do	
 	-- Infer kinds for this module
	let (treeHeader', treeSource', constraints, kindMap, errors)	
		= D.inferKindsTree
			unique
	 		treeHeader
			treeSource

	dumpST DumpDesugarKinds "desugar-kinds--constraints"
		(map ppr $ Foldable.toList constraints)

	dumpST DumpDesugarKinds "desugar-kinds--kindMap"
		(map (\(v, k) -> padL 20 v % " :: " % k % "\n") 
			$ Map.toList kindMap)

	dumpST DumpDesugarKinds "desugar-kinds--header" 
		(map (D.transformN $ \a -> (Nothing :: Maybe ())) treeHeader')

	dumpST DumpDesugarKinds "desugar-kinds--source" 
		(map (D.transformN $ \a -> (Nothing :: Maybe ())) treeSource')

	when (not $ null errors)
	 $ exitWithUserError ?args errors
					
	return	( treeHeader'
		, treeSource'
		, kindMap)

-- Elaborate ---------------------------------------------------------------------------------------
desugarElaborate
	:: (?args :: [Arg]
	 ,  ?pathSourceBase :: FilePath)
	=> String				-- ^ unique
	-> D.Tree SourcePos			-- ^ header tree
	-> D.Tree SourcePos			-- ^ source tree
	-> IO	( D.Tree SourcePos
		, D.Tree SourcePos )

desugarElaborate unique treeHeader treeSource
 = do	
	let treeHeader'	= D.elaborateTree (unique ++ "H") treeHeader
	let treeSource' = D.elaborateTree (unique ++ "S") treeSource

	dumpST DumpDesugarElaborate "desugar-elaborate--header"
		(map (D.transformN $ \a -> (Nothing :: Maybe ())) treeHeader')
	
	dumpST DumpDesugarElaborate "desugar-elaborate--source"
		(map (D.transformN $ \a -> (Nothing :: Maybe ())) treeSource')
		
	return	( treeHeader'
		, treeSource')
	
-- ProjectEta --------------------------------------------------------------------------------------
desugarProjectEta
	:: (?args :: [Arg]
	 ,  ?pathSourceBase :: FilePath)
	=> String
	-> D.Tree SourcePos
	-> IO	(D.Tree SourcePos)
	
desugarProjectEta unique sourceTree
 = do
	let sourceTree'	= D.projectEtaExpandTree unique sourceTree
	
	dumpST DumpDesugarProject "desugar-project-eta"
		(map (D.transformN $ \a -> (Nothing :: Maybe ())) sourceTree')

	return sourceTree'
	
-- Project -----------------------------------------------------------------------------------------
desugarProject 
	:: (?args :: [Arg]
	 ,  ?pathSourceBase :: FilePath)
	=> String
	-> ModuleId
	-> D.Tree SourcePos
	-> D.Tree SourcePos
	-> IO	( D.Tree SourcePos
		, D.ProjTable )

desugarProject unique moduleName headerTree sourceTree
 = do
	-- Snip down projection dictionaries and add default projections.
 	let (sourceTree', errors)
		= D.projectTree unique moduleName headerTree sourceTree 
	
	dumpST DumpDesugarProject "desugar-project"
		(map (D.transformN $ \a -> (Nothing :: Maybe ())) sourceTree')

	-- Slurp out the ProjTable
	let projTable	= D.slurpProjTable (headerTree ++ sourceTree')

	dumpS  DumpDesugarProject "desugar-project--dicts"
		(pprStrPlain $ "\n" %!% (map ppr $ Map.toList projTable))

	when (not $ null errors)
	 $ exitWithUserError ?args errors
		
	return (sourceTree', projTable)

	
	
-- Constraints -------------------------------------------------------------------------------------
desugarSlurpConstraints 	
	:: (?args :: [Arg]
	 ,  ?pathSourceBase :: FilePath)
	=> (D.Tree SourcePos)				-- source tree
	-> (D.Tree SourcePos)				-- header tree
	-> IO	( (D.Tree (Maybe (T.Type, T.Effect)))	-- source tree with type and effect annotations
		, [N.CTree]				-- type constraints
		, Map Var Var 				-- sigma table (map of value -> type vars)
		, Set Var				-- all the value vars used in the program that we'll want types for
		, Set Var)				-- type vars for all top level bindings in the source module
		
desugarSlurpConstraints
	sTree
	hTree

 = {-# SCC "slurpC" #-}
   do
	let state	= D.initCSlurpS 
	
	-- slurp constraints from the header
	let ((_, hctrs, _), state2)
			= runState (D.slurpTreeM hTree)
			$ state
		
	-- slurp constraints from the source 
	let ((source', sctrs, vsBoundTopLevel), state3)
			= runState (D.slurpTreeM sTree)
			$ state2

	-- handle errors arrising from constraint slurping
	when (not $ null $ D.stateErrors state3)
	 $ exitWithUserError ?args $ D.stateErrors state3

	-- these are the vars we'll need types for during the Core->Desugar transform
	let vsTypesPlease = D.stateTypesRequest state3

	-- this is the table mapping value vars to type vars
	let sigmaTable	= D.stateVarType state3

	-- simplify source constraints (header constraints are already pretty simple)
	let sctrs_simple = N.simplify vsTypesPlease sctrs


	-- dump
	let pprMode	= catMaybes $ map takePrettyModeOfArg ?args
	dumpST	DumpDesugarSlurp "desugar-slurp" source'
	
	dumpS	DumpTypeConstraints "type-constraints--source"
		$ (catInt "\n" $ map (pprStr pprMode) sctrs)

	dumpS	DumpTypeConstraints "type-constraints--source-simple"
		$ (catInt "\n" $ map (pprStr pprMode) sctrs_simple)
		
	dumpS	DumpTypeConstraints "type-constraints--header"
		$ (catInt "\n" $ map (pprStr pprMode) hctrs)

	dumpS	DumpTypeConstraints "type-constraints--typesPlease"
		$ (catInt "\n" $ map (pprStr pprMode) $ Set.toList vsTypesPlease)
		
	-- all the constraints we're passing to the inferencer
	let constraints	= hctrs ++ sctrs_simple

	--
	return	( source'
		, constraints
		, sigmaTable 
		, vsTypesPlease
		, vsBoundTopLevel)
	
	
-- Solve -------------------------------------------------------------------------------------------

desugarSolveConstraints
	:: (?args :: [Arg]
	 ,  ?pathSourceBase :: FilePath)
	=> [N.CTree]		-- type constraints
	-> Set Var		-- the TEC vars to infer TECs for	
	-> Set Var		-- type vars of value vars bound at top level
	-> Map Var Var		-- sigma table
	-> Bool			-- whether to require the 'main' function to have () -> () type
	
	-> IO 	( Map Var T.Type			-- inferred types
		, Map Var (T.InstanceInfo T.Type)	-- how each var was instantiated
		, Map Var (T.Kind, Maybe T.Type)	-- the vars that were quantified during type inference
							--	(with optional :> bound)
		, Set Var				-- the TREC vars which are free in the returned types
		, Map Var [Var]				-- map of constraints on each region
		, Map Var Var)				-- how projections were resolved
	
desugarSolveConstraints
	constraints
	vsTypesPlease
	vsBoundTopLevel
	sigmaTable
	blessMain

 = {-# SCC "solveSquid" #-}
   do
	-- The solver state gets dumped in real-time so we can see
	--	what's gone wrong if it crashes mid-stream.

	hTrace	<- dumpOpen DumpTypeSolve "type-solve--trace"
		
 	state	<- {-# SCC "solveSquid/solve" #-} T.squidSolve 
			?args		constraints
			sigmaTable	vsBoundTopLevel	
			hTrace		blessMain

	-- dump out the type graph
	--	do this before bailing on errors so we can see what's gone wrong.
	when (elem Verbose ?args)
	 $ putStr "  * Type: Dumping graph\n"
	 
	dGraph	<- evalStateT T.dumpGraph state
	dumpS	DumpTypeSolve  "type-solve--graph" (pprStrPlain dGraph)

	-- stop the compiler and print out errors
	--	if there were any during type inference.
	case T.stateErrors state of

	 -- no errors, carry on
	 [] -> desugarSolveConstraints2 
	 		vsTypesPlease 
			hTrace 
			state

	 -- time to die
	 _ -> do
		-- flush the trace output to make sure it's written to the file.
		(case hTrace of
		 Just handle	-> hFlush handle
		 Nothing	-> return ())

		when (not $ null $ T.stateErrors state)
		 $ exitWithUserError ?args $ T.stateErrors state
		
		panic "Core.SolveSquid" "done already"

	 
desugarSolveConstraints2 
	vsTypesPlease 
	hTrace 
	state
 = do	
	when (elem Verbose ?args)
	 $ putStr "  * Type: Exporting\n"

	-- extract out the stuff we'll need for conversion to core.
	(junk, state2)	<- {-# SCC "solveSquid/export" #-} runStateT 
				(T.squidExport vsTypesPlease) state

	let (typeTable, typeInst, quantVars, vsRegionClasses)
			= junk

	-- flush the trace output to make sure it's written to the file.
	(case hTrace of
	 Just handle	-> hFlush handle
	 Nothing	-> return ())

	-- report some state
	when (elem Verbose ?args)
	 $ do	graph	<- readIORef (T.stateGraph state)
		putStr 	$ pprStrPlain
	 		$ "    - graph size: " 
	 		% T.graphClassIdGen graph % "\n"

	when (elem Verbose ?args)
	 $ putStr "  * Type: Dumping final state\n"

	-- dump final solver state
	dumpS	DumpTypeSolve  "type-solve--types"
		$ catInt "\n\n"
		$ map pprStrPlain
		$ map (\(v, t) -> v % " ::\n" %> T.prettyTypeSplit t)
		$ Map.toList typeTable

	dumpS 	DumpTypeSolve   "type-solve--inst" 
		$ catInt "\n\n"
		$ map pprStrPlain
		$ map (\(v, inst) -> v % "\n" % inst % "\n")
		$ Map.toList typeInst

	dumpS	DumpTypeSolve	"type-solve--quantVars"
		$ catInt "\n"
		$ map pprStrPlain
		$ Map.toList quantVars

	dumpS	DumpTypeSolve	"type-solve--regionClasses"
		$ catInt "\n"
		$ map pprStrPlain
		$ Map.toList vsRegionClasses

	varSub	<- readIORef (T.stateVarSub state2)
	dumpS	DumpTypeSolve	"type-solve--varSub"
		$ catInt "\n"
		$ map pprStrPlain
		$ Map.toList varSub

	projRes <- readIORef (T.stateProjectResolve state2)
	dumpS	DumpTypeSolve	"type-solve--projResolve"
		$ catInt "\n"
		$ map pprStrPlain
		$ Map.toList projRes

	let vsFree	= Set.empty

	-- the export process can find more errors
	when (not $ null $ T.stateErrors state2)
	 $ exitWithUserError ?args $ T.stateErrors state2

	-----
	return 	( typeTable
		, typeInst
		, quantVars
		, vsFree
		, vsRegionClasses
		, projRes)



-- ToCore ------------------------------------------------------------------------------------------

desugarToCore 
	:: (?args :: [Arg]
	 ,  ?pathSourceBase :: FilePath)
	=> D.Tree (Maybe (T.Type, T.Effect))	-- sourceTree
	-> D.Tree (Maybe (T.Type, T.Effect))	-- headerTree
	-> Map Var Var				-- sigmaTable
	-> Map Var T.Type			-- typeTable
	-> Map Var (T.InstanceInfo T.Type)	-- typeInst
	-> Map Var (T.Kind, Maybe T.Type)	-- typeQuantVars
	-> D.ProjTable				-- projection dictinary
	-> Map Var Var				-- how to resolve projections
	-> IO	( C.Tree
		, C.Tree )

desugarToCore	
	sourceTree
	headerTree
	sigmaTable
	typeTable
	typeInst
	quantVars
	projTable
	projResolve
 = {-# SCC "toCore" #-} 
   do
	-----
	let toCoreTree'	
		= D.toCoreTree
			sigmaTable
			typeTable
			typeInst
			quantVars
			projTable
			projResolve
			
 	let cSource	= toCoreTree' sourceTree
	let cHeader	= toCoreTree' headerTree

	-----
	dumpCT DumpCore "core-source" cSource
	dumpCT DumpCore "core-header" cHeader


	return	( cSource
		, cHeader )
