module Main.Desugar
	( desugarElaborate
	, desugarProjectEta
	, desugarProject
	, desugarSlurp
	, desugarSolve
	, desugarToCore )
where
import Main.Dump
import DDC.Main.Pretty
import DDC.Main.Arg
import DDC.Main.Error
import DDC.Base.SourcePos
import DDC.Var
import Data.IORef
import Control.DeepSeq
import System.IO				(hFlush)
import Util					hiding (null, elem)
import qualified DDC.Type			as T
import qualified Data.Foldable			as Foldable
import qualified DDC.Core.Glob			as C
import qualified Type.Export			as T
import qualified Type.Dump			as T
import qualified DDC.Solve			as T
import qualified DDC.Solve.Interface.Problem	as T
import qualified DDC.Solve.State		as T
import qualified DDC.Solve.Error.Beautify	as T
import qualified DDC.Constraint.Simplify	as T
import qualified DDC.Desugar.Glob		as D
import qualified DDC.Desugar.Exp		as D
import qualified DDC.Desugar.ToCore		as D
import qualified DDC.Desugar.ToCore.Clean	as D
import qualified DDC.Desugar.Elaborate		as D
import qualified DDC.Desugar.ProjectEta		as D
import qualified DDC.Desugar.Slurp.Slurp	as D
import qualified Desugar.Project		as D
import qualified DDC.Desugar.Transform		as D
import qualified Data.Map			as Map
import qualified Data.Set			as Set


-- Elaborate ---------------------------------------------------------------------------------------
-- | Elaborate type information.
desugarElaborate
	:: (?args :: [Arg]
	 ,  ?pathSourceBase :: FilePath)
	=> String				-- ^ unique
	-> D.Glob SourcePos			-- ^ header tree
	-> D.Glob SourcePos			-- ^ source tree
	-> IO	( D.Glob SourcePos
		, D.Glob SourcePos
		, Map Var T.Kind)

desugarElaborate unique dgHeader dgModule
 = {-# SCC "D/elaborate" #-}
   do	
	let (dgHeader', dgModule', constraints, kindMap, errors)
		= D.elaborateTree unique dgHeader dgModule
		
	let treeHeader'	= D.treeOfGlob dgHeader'
	let treeModule'	= D.treeOfGlob dgModule'	
		
	dumpST DumpDesugarElaborate "desugar-elaborate--header"
		(map (D.transformN $ \a -> (Nothing :: Maybe ())) treeHeader')
	
	dumpST DumpDesugarElaborate "desugar-elaborate--source"
		(map (D.transformN $ \a -> (Nothing :: Maybe ())) treeModule')

	dumpST DumpDesugarElaborate "desugar-elaborate--constraints"
		(map ppr $ Foldable.toList constraints)

	dumpST DumpDesugarElaborate "desugar-elaborate--kinds"
		(map (\(v, k) -> padL 20 v % " :: " % k % "\n") 
			$ Map.toList kindMap)

	when (not $ null errors)
	 $ exitWithUserError ?args errors
		
	return	( dgHeader'
		, dgModule'
		, kindMap)
		
		
-- ProjectEta --------------------------------------------------------------------------------------
desugarProjectEta
	:: (?args :: [Arg]
	 ,  ?pathSourceBase :: FilePath)
	=> String
	-> D.Tree SourcePos
	-> IO	(D.Tree SourcePos)
	
desugarProjectEta unique sourceTree
 = {-# SCC "D/projectEta" #-}
   do
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

desugarProject unique modName headerTree sourceTree
 = {-# SCC "D/project" #-}
   do
	-- Snip down projection dictionaries and add default projections.
 	let (sourceTree', errors)
		= D.projectTree unique modName headerTree sourceTree 
	
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
desugarSlurp	
	:: (?args :: [Arg]
	 ,  ?pathSourceBase :: FilePath)
	=> Bool						-- whether to require main fn to have type () -> ()
	-> (D.Tree SourcePos)				-- source tree
	-> (D.Tree SourcePos)				-- header tree
	-> IO	( (D.Tree (Maybe (T.Type, T.Effect)))	-- source tree with type and effect annotations
		, T.Problem)				-- problem for contraint solver
				
desugarSlurp blessMain sTree hTree
 = do
	let (sTree', problem, errs)	
		= {-# SCC "D/slurp/slurp" #-}
		  D.slurpTree blessMain hTree sTree

	-- handle errors arrising from constraint slurping
	when (not $ null errs)
	 $ exitWithUserError ?args errs

	-- Run the constraint simplifier unless we were told not to.
	-- This tidies up the constraints before solving, which makes the final
	-- constraints easier to read. It's also a small performance boost,
	-- but is otherwise an optional pass.
	(problem_simplified, mUsage)
	 <- if elem DebugNoConstraintSimplifier ?args
	     then return (problem, Nothing)
	     else do
		(simplified, usage)
		 <- {-# SCC "D/slurp/simplify" #-}
			  T.simplify 	(T.problemTypeVarsPlease problem)
					(T.problemConstraints    problem)
		return  ( problem { T.problemConstraints = simplified }
			, Just usage )

	-- dumping.
	let pprMode	= catMaybes $ map takePrettyModeOfArg ?args
	dumpST	DumpDesugarSlurp "desugar-slurp" sTree'
	
	dumpS	DumpTypeConstraints "type-constraints"
		$ pprStr pprMode  $ T.problemConstraints problem

	when (not $ elem DebugNoConstraintSimplifier ?args)
	 $ do	dumpS DumpTypeConstraints "type-constraints--simpified"
		 $ pprStr pprMode $ T.problemConstraints problem_simplified

		let Just usage	= mUsage
		dumpS DumpTypeConstraints "type-constraints--usage"
		 $ (pprStr pprMode usage)


	dumpS	DumpTypeConstraints "type-constraints--typesPlease"
		$ (catInt "\n" $ map (pprStr pprMode) $ Set.toList $ T.problemTypeVarsPlease problem_simplified)
	
	return	$ deepSeq (T.problemConstraints problem_simplified)
		$ ( sTree'
		  , problem_simplified)
	
	
-- Solve -------------------------------------------------------------------------------------------
desugarSolve
	:: (?args :: [Arg]
	 ,  ?pathSourceBase :: FilePath)
	=> T.Problem		-- ^ Problem for type constraint solver.
	-> IO T.Solution
	
desugarSolve problem
 = {-# SCC "D/solve" #-}
   do
	-- The solver state gets dumped in real-time so we can see
	--	what's gone wrong if it crashes mid-stream.

	hTrace	<- dumpOpen DumpTypeSolve "type-solve--trace"
		
 	state	<- {-# SCC "D/solve/solve" #-} 
		   T.solveProblem ?args hTrace problem

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
			hTrace
			problem 
	 		(T.problemTypeVarsPlease problem)
			state

	 -- time to die
	 _ -> do
		-- flush the trace output to make sure it's written to the file.
		(case hTrace of
		 Just handle	-> hFlush handle
		 Nothing	-> return ())

		when (not $ null $ T.stateErrors state)
		 	$ exitWithUserError ?args 
			$ T.beautifyErrors
			$ T.stateErrors state
		
		panic "Core.SolveSquid" "done already"

	 
desugarSolveConstraints2 
	hTrace 
	problem
	vsTypesPlease 
	state
 = do	
	when (elem Verbose ?args)
	 $ putStr "  * Type: Exporting\n"

	-- extract out the stuff we'll need for conversion to core.
	(solution, state2)	
		<- {-# SCC "D/solve/export" #-} runStateT 
			(T.squidExport vsTypesPlease) state

	-- flush the trace output to make sure it's written to the file.
	(case hTrace of
	 Just handle	-> hFlush handle
	 Nothing	-> return ())

	-- report the size of the graph.
	when (elem Verbose ?args)
	 $ do	graph	<- readIORef (T.stateGraph state)
		size	<- readIORef (T.graphClassIdGen graph)
		putStr 	$ pprStrPlain
	 		$ "    - graph size: " 
	 		% size % "\n"

	-- dump details of the solution.
	dumpS	DumpTypeSolve  "type-solution--types"
		$ catInt "\n\n"
		$ map pprStrPlain
		$ map (\(v, t) -> v % " ::\n" %> T.prettyTypeSplit t)
		$ Map.toList 
		$ T.solutionTypes solution

	dumpS 	DumpTypeSolve   "type-solution--instanceInfo" 
		$ catInt "\n\n"
		$ map pprStrPlain
		$ map (\(v, inst) -> v % "\n" % inst % "\n")
		$ Map.toList 
		$ T.solutionInstanceInfo solution

	dumpS	DumpTypeSolve	"type-solution--regionClasses"
		$ catInt "\n"
		$ map pprStrPlain
		$ Map.toList
		$ T.solutionRegionClasses solution
		
	dumpS	DumpTypeSolve	"type-soltion--projResolution"
		$ catInt "\n"
		$ map pprStrPlain
		$ Map.toList
		$ T.solutionProjResolution solution

	-- the export process can find more errors
	when (not $ null $ T.stateErrors state2)
	 	$ exitWithUserError ?args 
		$ T.beautifyErrors 
		$ T.stateErrors state2
		
	return 	solution


-- ToCore ------------------------------------------------------------------------------------------
desugarToCore 
	:: (?args :: [Arg]
	 ,  ?pathSourceBase :: FilePath)
	=> D.Tree (Maybe (T.Type, T.Effect))	-- ^ Source tree
	-> D.Tree (Maybe (T.Type, T.Effect))	-- ^ Header tree.
	-> Map Var Var				-- ^ value -> type vars.
	-> D.ProjTable				-- ^ projection dictionaries.
	-> T.Solution				-- ^ solution from constraint solver.
	-> IO	( C.Glob
		, C.Glob )

desugarToCore 
	sourceTree
	headerTree
	mapValueToTypeVars
	projTable
	solution

 = {-# SCC "D/toCore" #-} 
   do 	let toCoreGlob'
		= D.toCoreTree 
			mapValueToTypeVars
			projTable
			solution
	
	let cgSource		= toCoreGlob' sourceTree
	let cgSource_clean	= D.cleanGlob cgSource
	let cgHeader		= toCoreGlob' headerTree

	dumpCT DumpCore "core-header"       $ C.treeOfGlob cgHeader
	dumpCT DumpCore "core-source"       $ C.treeOfGlob cgSource
	dumpCT DumpCore "core-source-clean" $ C.treeOfGlob cgSource_clean

	return	( cgSource_clean
		, cgHeader )


