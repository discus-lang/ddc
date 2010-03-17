{-# OPTIONS -fwarn-unused-imports #-}

module Main.Compile
	( compileFile )
where

-- This is the module which ties the whole compilation pipeline together
--	so there'll be a lot of imports...

-- main stages
import Main.Invoke
import Main.Setup
import Main.BuildFile
import Main.Error
import qualified Main.Arg		as Arg
import qualified Main.Dump		as Dump
import qualified Main.Source		as SS
import qualified Main.Desugar		as SD
import qualified Main.Core		as SC
import qualified Main.Sea		as SE

-- module
import qualified Module.Export		as M
import qualified Module.ExportNew	as MN
import qualified Module.Scrape		as M

-- source
import qualified Source.Exp		as S
import qualified Source.Pragma		as Pragma

-- desugar
import qualified Desugar.Plate.Trans	as D

-- type
import qualified Type.Plate.FreeVars	as T

-- core
import qualified Core.Glob		as C
import qualified Core.Util		as C

-- sea
import qualified Sea.Util		as E

-- shared
import Shared.Var			(Module(..), NameSpace(..))
import Shared.Error
import Shared.Pretty
import qualified Shared.Var		as Var

-- haskell
import Util
import Util.FilePath
import qualified Data.Map		as Map
import qualified Data.Set		as Set
import qualified System.Exit		as System


out ss		= putStr $ pprStrPlain ss

outVerb :: (?verbose :: Bool) => PrettyM PMode -> IO ()
outVerb ss	= when ?verbose (putStr $ pprStrPlain ss)


-- Compile -----------------------------------------------------------------------------------------
-- |	Top level compilation function.
--	Compiles one source file.
--	Returns a list of module names and the paths to their compiled object files.
--
compileFile 
	:: Setup 		-- ^ compile setup.
	-> Map Module M.Scrape	-- ^ scrape graph of all modules reachable from the root.
	-> Module		-- ^ module to compile, must also be in the scrape graph.
	-> Bool			-- ^ whether to treat a 'main' function defined by this module
				--	as the program entry point.
	-> IO Bool		-- ^ true if the module defines the main function

compileFile setup scrapes sModule blessMain
 = do 	let ?verbose	= elem Arg.Verbose (setupArgsCmd setup)

	-- Decide on module names  ---------------------------------------------
	let Just sRoot	= Map.lookup sModule scrapes
	let pathSource	= let Just s = M.scrapePathSource sRoot in s
	let (fileName, fileDir, _, _)
		= normalMunchFilePath pathSource

	-- Gather up all the import dirs.
	let importDirs	
		= (setupLibrary setup)
		: fileDir
		: (concat $ [dirs | Arg.ImportDirs dirs <- setupArgs setup])

	when ?verbose
	 $ do	out	$ "  * CompileFile " % (M.scrapePathSource sRoot) % "\n"
			% "    - setup  = " % show setup 	% "\n\n"
			% "    - sRoot  = " % show sRoot	% "\n\n"

	 	out	$ "  * Compile options are:\n"
			% (concat 
				$ map (\s -> "    " ++ s ++ "\n") 
				$ map show $ setupArgs setup)
			% "\n"
			
	-- Load the source file
	sSource		<- readFile fileName

	compileFile_parse 
		setup
		scrapes
		sRoot
		sModule
		importDirs 
		blessMain
		sSource


compileFile_parse
	setup
	scrapes
	sRoot
	sModule
	importDirs
	blessMain
	sSource

 -- Emit a nice error message if the source file is empty.
 --	If we parse an empty file to happy it will bail with "reading EOF!" which isn't as nice.
 | words sSource == []
 = let Just pathSource	= M.scrapePathSource sRoot
   in  exitWithUserError (setupArgs setup) 
   	[pathSource % "\n    Source file is empty.\n"]

 | otherwise
 = do
	let ?args		= setupArgs setup
	let ?verbose		= elem Arg.Verbose ?args

	-- extract some file names
	let modName		= M.scrapeModuleName sRoot

	let pathSource		= let Just s = M.scrapePathSource sRoot in s

	let Just (fileDir, fileBase, _) 
				= munchFileName pathSource
	let ?pathSourceBase	= fileDir ++ "/" ++ fileBase
	
	-- Parse imported interface files --------------------------------------
	let loadInterface (mod, scrape) = 
	     case M.scrapePathInterface scrape of
	      Nothing	-> panic "Main.Compile" $ "compileFile: no interface for " % mod % "\n"
	      Just path	-> do
		src		<- readFile path
		(tree, _)	<- SS.parse path src
		return	(mod, tree)
		
	let scrapes_noRoot	= Map.delete (M.scrapeModuleName sRoot) scrapes 
	importsExp		<- liftM Map.fromList
				$ mapM loadInterface 
				$ Map.toList scrapes_noRoot

	Dump.dumpST 	Arg.DumpSourceParse "source-parse--header" 
		(concat $ Map.elems importsExp)


	-- Dump a nice graph of the module hierarchy.
{-	let modulesCut	= concat
			$ [ss | Arg.GraphModulesCut ss <- ?args]
			
	SD.dumpDot Arg.GraphModules
		"modules"
		(M.dotModuleHierarchy 
			moduleName
			modulesCut 
			importsRoot
			importsExp)
-}

	-- Parse the source file.
	outVerb $ ppr $ "  * Source: Parse\n"
	(sParsed, pragmas)	
			<- {-# SCC "Main.parsed" #-} SS.parse pathSource sSource

	outVerb	$ "  * Pragmas\n"
		%> punc "\n" pragmas
		% "\n\n"

	-- Slurp out pragmas
	let pragmas	= Pragma.slurpPragmaTree sParsed

	-- Chase down extra C header files to include into source via pragmas.
	let includeFilesHere	= [ str | Pragma.PragmaCCInclude str <- pragmas]
	
	when ?verbose
	 $ do	mapM_ (\path -> putStr $ pprStrPlain $ "  - included file   " % path % "\n")
	 		$ includeFilesHere
	 	

	------------------------------------------------------------------------
	-- Source Stages
	------------------------------------------------------------------------

	-- slurp out name of vars to inline
	inlineVars	<- SS.sourceSlurpInlineVars sParsed
	
	-- Rename variables and add uniqueBinds.
	outVerb $ ppr $ "  * Source: Rename\n"
	((_, sRenamed) : modHeaderRenamedTs)
			<- {-# SCC "Main.renamed" #-}
				SS.rename
					$ (modName, sParsed) : Map.toList importsExp
	
	let hRenamed	= concat 
			$ [tree	| (mod, tree) <- modHeaderRenamedTs ]


	-- If this module is Main.hs then require it to contain the main function.
	let modDefinesMainFn 
			= any (\p -> case p of
					S.PStmt (S.SBindFun _ v _ _)
					 | Var.name v == "main"	-> True
					 | otherwise		-> False
					_			-> False)
			$ sRenamed
	
	when (  sModule == ModuleAbsolute ["Main"]
	    &&  (not $ modDefinesMainFn))
		 $ exitWithUserError ?args [ErrorNoMainInMain]
					
			
	-- Slurp out header information and fixity defs.
	fixTable	<- SS.sourceSlurpFixTable
				(sRenamed ++ hRenamed)
	
	-- Defix the source.
	outVerb $ ppr $ "  * Source: Defix\n"
	sDefixed	<- SS.defix
				sRenamed
				fixTable

	-- Slurp out kind table
	kindTable	<- SS.sourceKinds (sDefixed ++ hRenamed)
	
	-- Lint check the source program before desugaring
	(hRenamed_ok, sDefixed_ok)	
			<- SS.lint hRenamed sDefixed
	
	-- Desugar the source language
	outVerb $ ppr $ "  * Source: Desugar\n"
	(hDesugared, sDesugared)
			<- {-# SCC "Main.desugared" #-}
			   SS.desugar
				"SD"
				kindTable
				hRenamed_ok
				sDefixed_ok

	------------------------------------------------------------------------
	-- Desugar/Type inference stages
	------------------------------------------------------------------------

	-- Do kind inference and tag all type constructors with their kinds
	outVerb $ ppr $ "  * Desugar: Infer Kinds\n"
	(hKinds, sKinds, kindTable)
		<- SD.desugarInferKinds "DK" hDesugared sDesugared
	
	-- Elaborate effects and closures of types in foreign imports
	outVerb $ ppr $ "  * Desugar: Elaborate\n"
	(hElab, sElab)
		<- SD.desugarElaborate "DE" hKinds sKinds

	-- Eta expand simple v1 = v2 projections
	outVerb $ ppr $ "  * Desugar: Project Eta-Expand\n"
	sProjectEta
		<- SD.desugarProjectEta "DE" sElab
			
	-- Snip down dictionaries and add default projections.
	outVerb $ ppr $ "  * Desugar: Project Snip\n"
	(dProject, projTable)	
		<- SD.desugarProject "SP" modName hElab sProjectEta

	-- Slurp out type constraints.
	outVerb $ ppr $ "  * Desugar: Slurp\n"

	(  sTagged
	 , sConstrs
	 , sigmaTable
	 , vsTypesPlease
	 , vsBoundTopLevel)
			<- SD.desugarSlurpConstraints
				dProject
				hElab

	let hTagged	= map (D.transformN (\n -> Nothing)) hElab

	-- !! Early exit on StopConstraint
	when (elem Arg.StopConstraint ?args)
		compileExit

	outVerb $ ppr "  * Type: Solve\n"

	-- Solve type constraints
	(  typeTable
	 , typeInst
	 , typeQuantVars
	 , vsFree
	 , vsRegionClasses 
	 , vsProjectResolve)
	 		<- runStage "solve"
			$  SD.desugarSolveConstraints
				sConstrs
				vsTypesPlease
				vsBoundTopLevel
				sigmaTable
				blessMain

	-- !! Early exit on StopType
	when (elem Arg.StopType ?args)
		compileExit

	outVerb $ ppr $ "  * Desugar: ToCore\n"
		
	-- Convert source tree to core tree
	(  cSource
	 , cHeader )	<- runStage "core"
	 		$  SD.desugarToCore
		 		sTagged
				hTagged
				sigmaTable
				typeTable
				typeInst
				typeQuantVars
				projTable
				vsProjectResolve

	-- Slurp out the list of all the vars defined at top level.
	let topVars	= Set.union 
				(Set.fromList $ concat $ map C.slurpBoundVarsP cSource)
				(Set.fromList $ concat $ map C.slurpBoundVarsP cHeader)	

	-- These are the TREC vars which are free in the type of a top level binding
	let vsFreeTREC	= Set.unions
			$ map (T.freeVars)
			$ [t	| (v, t)	<- Map.toList typeTable
				, Set.member v vsBoundTopLevel]


	------------------------------------------------------------------------
	-- Core stages
	------------------------------------------------------------------------	

	-- Convert to normal form
	outVerb $ ppr $ "  * Core: Normalise Do Exprs\n"

	cNormalise	<- SC.coreNormalDo "core-normaldo" "CN" cSource
				
	-- Create local regions.
	when ?verbose
	 $ do	putStr $ "  * Core: Bind\n"

	-- these regions have global scope
	let rsGlobal	= Set.filter (\v -> Var.nameSpace v == NameRegion) 
			$ vsFreeTREC
	
	cBind		<- SC.coreBind sModule "CB"
				vsRegionClasses
				rsGlobal
				cNormalise

	-- Convert to A-normal form
	cSnip		<- SC.coreSnip "core-snip" "CS" topVars cBind

	-- Thread through witnesses
	cThread		<- SC.coreThread cHeader cSnip

	-- Check type information and add annotations to each stmt.
	when ?verbose
	 $ do	putStr $ "  * Core: Reconstruct\n"

	cReconstruct	<- runStage "reconstruct"
			$ SC.coreReconstruct "core-reconstruct" cHeader cThread

	-- lint: 
	when ?verbose
	 $ do	putStr $ "  * Core: Lint\n"

	SC.coreLint "core-lint-reconstruct" cReconstruct cHeader
	
	
	-- Call class instance functions and add dictionaries.
	when ?verbose
	 $ do	putStr $ "  * Core: Dict\n"

	cDict		<- SC.coreDict 
				cHeader
				cReconstruct

	-- Identify primitive operations
	cPrim		<- SC.corePrim	cDict

	------------------------------------------------------------------------
	-- Core stages
	------------------------------------------------------------------------	

	when ?verbose
	 $ do	putStr $ "  * Core: Optimise\n"

	-- Inline forced inline functions
	cInline		<- SC.coreInline
				cPrim
				cHeader
				inlineVars

	-- Perform simplification
	cSimplify	<- if elem Arg.OptSimplify ?args
				then SC.coreSimplify "CI" topVars cInline cHeader
				else return cInline
	
	-- Do the full laziness optimisation.
	cFullLaziness	<- SC.coreFullLaziness 
				modName
				cSimplify
				cHeader

	-- Check the program one last time
	--	Lambda lifting doesn't currently preserve the typing.
	cReconstruct_final	
		<- runStage "reconstruct-final"
		$  SC.coreReconstruct  "core-reconstruct-final" cHeader cFullLaziness

	-- Convert to glob form.
	-- TODO: Use Glob for all of the core stages.
	let cgProg_prep		= C.globOfTree cReconstruct_final
	let cgHeader		= C.globOfTree cHeader
	
	-- Slurp out ctor defs
	let mapCtorDefs	= Map.union
				(C.slurpCtorDefs cReconstruct_final)
				(C.slurpCtorDefs cHeader)
			
	-- Perform lambda lifting.
	when ?verbose
	 $ do	putStr $ "  * Core: Lift\n"

	(  cLambdaLift
	 , vsLambda_new) <- SC.coreLambdaLift
				cReconstruct_final
				cHeader


--	Can't lint the code after lifting, lifter doesn't preserve
--	kinds of witness variables.
--  	runStage "lint-lifted" 
--		$ SC.coreLint "core-lint-lifted" cLambdaLift cHeader


	-- Convert field labels to field indicies
	cLabelIndex	<- SC.coreLabelIndex
				mapCtorDefs
				cLambdaLift

	let cgProg_labelIndex	= C.globOfTree cLabelIndex

				
	-- Sequence bindings and CAFs
	cSequence	<- SC.coreSequence	
				cLabelIndex
				cHeader
					
	-- Resolve partial applications.
	cCurry		<- SC.curryCall
				cSequence
				cHeader
				cgProg_labelIndex	-- for super arities.
				cgHeader		-- TODO: refactor to just take the globs.
				
	-- Rewrite so atoms are shared.
	cAtomise	<- if elem Arg.OptAtomise ?args
				then SC.coreAtomise cCurry cHeader
				else return cCurry
	
	let cgProg_final	= C.globOfTree cAtomise
	
	-- Generate the module interface
	--
	diInterface	<- M.makeInterface
				modName
				sRenamed
				dProject
				cAtomise
				sigmaTable
				typeTable
				vsLambda_new

	writeFile (?pathSourceBase ++ ".di") diInterface	

	-- Make the new style module interface.
	let Just thisScrape	
			= Map.lookup modName scrapes

	let diNewInterface	
	 		= MN.makeInterface
				thisScrape

--	writeFile (?pathSourceBase ++ ".di-new") 
--		$ show diNewInterface


	-- !! Early exit on StopCore
	when (elem Arg.StopCore ?args)
		compileExit

	-- Convert to Sea code.
	-- Perform lambda lifting.
	when ?verbose
	 $ do	putStr $ "  * Core: ToSea\n"

	(eSea, eHeader)	<- SC.toSea
				"TE"
				cAtomise
				cHeader
				
	------------------------------------------------------------------------
	-- Sea stages
	------------------------------------------------------------------------
		
	-- Eliminate simple v1 = v2 statements.
	eSub		<- SE.seaSub
				eSea
				
	-- Expand out constructors.
	eCtor		<- SE.seaCtor
				eSub
				
	-- Expand out thunking.
	eThunking	<- SE.seaThunking
				eCtor
				
	-- Add suspension forcing code.
	eForce		<- SE.seaForce
				eThunking
				
	-- Add GC slots and fixup calls to CAFS
	eSlot		<- SE.seaSlot	
				eForce
				eHeader
				cgHeader
				cgProg_final

	-- Flatten out match stmts.
	eFlatten	<- SE.seaFlatten
				"EF"
				eSlot

	-- Generate module initialisation functions.
	eInit		<- SE.seaInit
				modName
				eFlatten

	-- Generate C source code
	(  seaHeader
	 , seaSource )	<- SE.outSea
				modName
	 			eInit
				pathSource
				(map ((\(Just f) -> f) . M.scrapePathHeader) $ Map.elems scrapes_noRoot)
				includeFilesHere


	-- If this module binds the top level main function
	--	then append RTS initialisation code.
	seaSourceInit	<- if modDefinesMainFn && blessMain
				then do mainCode <- SE.seaMain (map fst $ Map.toList importsExp) modName
				     	return 	$ seaSource ++ (catInt "\n" $ map pprStrPlain $ E.eraseAnnotsTree mainCode)
				else 	return  $ seaSource
				
	writeFile (?pathSourceBase ++ ".ddc.c") seaSourceInit
	writeFile (?pathSourceBase ++ ".ddc.h") seaHeader


	------------------------------------------------------------------------
	-- Invoke external compiler / linker
	------------------------------------------------------------------------

	-- !! Early exit on StopSea
	when (elem Arg.StopSea ?args)
		compileExit	
	
	-- Invoke GCC to compile C source.
	invokeSeaCompiler
		?args
		?pathSourceBase
		(setupRuntime setup)
		(setupLibrary setup)
		importDirs
		(fromMaybe [] $ liftM buildExtraCCFlags (M.scrapeBuild sRoot))
	
	
	return modDefinesMainFn 
		
-----
compileExit
 = do 	System.exitWith System.ExitSuccess


runStage :: String -> IO a -> IO a
runStage name stage
 =	stage

	 	 