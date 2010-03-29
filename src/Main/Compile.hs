
module Main.Compile
	(compileFile)
where

-- This is the module which ties the whole compilation pipeline together
--	so there'll be a lot of imports...

-- main stages
import Main.Invoke
import Main.Setup
import Main.BuildFile
import Main.Error
import qualified Main.Dump		as Dump
import qualified Main.Source		as SS
import qualified Main.Desugar		as SD
import qualified Main.Core		as SC
import qualified Main.Sea		as SE
import qualified DDC.Main.Arg		as Arg

-- module
import qualified Module.Interface.Pretty ()
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
import DDC.Var
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Util.Doc

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
	:: Setup 			-- ^ compile setup.
	-> Map ModuleId M.Scrape	-- ^ scrape graph of all modules reachable from the root.
	-> ModuleId			-- ^ module to compile, must also be in the scrape graph.
	-> Bool				-- ^ whether to treat a 'main' function defined by this module
					--	as the program entry point.
	-> IO Bool			-- ^ true if the module defines the main function

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

	-- Parse the source file ----------------------------------------------
	outVerb $ ppr $ "  * Source: Parse\n"
	(sParsed, pragmas)	
			<- {-# SCC "Main.parsed" #-} SS.parse pathSource sSource

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
	
	-- Rename variables and add uniqueBinds -------------------------------
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
					 | varName v == "main"	-> True
					 | otherwise		-> False
					_			-> False)
			$ sRenamed
	
	when (  sModule == ModuleId ["Main"]
	    &&  (not $ modDefinesMainFn))
		 $ exitWithUserError ?args [ErrorNoMainInMain]
					
			
	-- Slurp out header information and fixity defs.
	fixTable	<- SS.sourceSlurpFixTable
				(sRenamed ++ hRenamed)
	

	-- Defix the source ---------------------------------------------------
	outVerb $ ppr $ "  * Source: Defix\n"
	sDefixed	<- SS.defix
				sRenamed
				fixTable

	-- Slurp out kind table
	kindTable	<- SS.sourceKinds (sDefixed ++ hRenamed)
	
	-- Lint check the source program before desugaring -------------------
	outVerb $ ppr $ "  * Source: Lint\n"

	(hRenamed_ok, sDefixed_ok)	
			<- SS.lint hRenamed sDefixed
	
	-- Desugar the source language ----------------------------------------
	outVerb $ ppr $ "  * Convert to Desugared IR\n"
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

	-- Do kind inference --------------------------------------------------
	outVerb $ ppr $ "  * Desugar: InferKinds\n"
	(hKinds, sKinds, kindTable)
		<- SD.desugarInferKinds "DK" hDesugared sDesugared

	
	-- Elaborate effects and closures of types in foreign imports ---------
	outVerb $ ppr $ "  * Desugar: Elaborate\n"
	(hElab, sElab)
		<- SD.desugarElaborate "DE" hKinds sKinds


	-- Eta expand simple v1 = v2 projections ------------------------------
	outVerb $ ppr $ "  * Desugar: ProjectEta\n"
	sProjectEta
		<- SD.desugarProjectEta "DE" sElab

			
	-- Snip down dictionaries and add default projections -----------------
	outVerb $ ppr $ "  * Desugar: Project\n"
	(dProject, projTable)	
		<- SD.desugarProject "SP" modName hElab sProjectEta


	-- Slurp out type constraints -----------------------------------------
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


	-- Solve type constraints ---------------------------------------------
	outVerb $ ppr "  * Type: Solve\n"

	(  typeTable
	 , typeInst
	 , typeQuantVars
	 , vsFree
	 , vsRegionClasses 
	 , vsProjectResolve)
			<- SD.desugarSolveConstraints
				sConstrs
				vsTypesPlease
				vsBoundTopLevel
				sigmaTable
				blessMain

	-- !! Early exit on StopType
	when (elem Arg.StopType ?args)
		compileExit

		
	-- Convert source tree to core tree -----------------------------------
	outVerb $ ppr $ "  * Convert to Core IR\n"

	(  cSource
	 , cHeader )	<- SD.desugarToCore
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

	-- Convert to normal form ---------------------------------------------
	outVerb $ ppr $ "  * Core: NormalDo\n"
	cNormalise	<- SC.coreNormalDo 
				"core-normaldo" "CN" 
				cSource

				
	-- Create local regions -----------------------------------------------
	outVerb $ ppr $ "  * Core: Bind\n"

	-- these regions have global scope
	let rsGlobal	= Set.filter (\v -> varNameSpace v == NameRegion) 
			$ vsFreeTREC
	
	cBind		<- SC.coreBind sModule "CB"
				vsRegionClasses
				rsGlobal
				cNormalise


	-- Convert to A-normal form -------------------------------------------
	outVerb $ ppr $ "  * Core: Snip\n"
	cSnip		<- SC.coreSnip "core-snip" "CS" topVars cBind


	-- Thread through witnesses -------------------------------------------
	outVerb $ ppr $ "  * Core: Thread\n"
	cThread		<- SC.coreThread cHeader cSnip


	-- Reconstruct and check types ----------------------------------------
	outVerb $ ppr $ "  * Core: Reconstruct\n"
	cReconstruct	<- SC.coreReconstruct 
				"core-reconstruct" 
				cHeader cThread


	-- Lint the core program ----------------------------------------------
	outVerb $ ppr $ "  * Core: Lint\n"

	SC.coreLint "core-lint-reconstruct"
	 	cReconstruct 
		cHeader
	
	
	-- Rewrite projections to use instances from dictionaries -------------
	outVerb $ ppr $ "  * Core: Dict\n"
	cDict		<- SC.coreDict 
				cHeader
				cReconstruct


	-- Identify prim ops --------------------------------------------------
	outVerb $ ppr $ "  * Core: Prim\n"
	cPrim		<- SC.corePrim	cDict


	-- Simplifier does various simple optimising transforms ---------------
	outVerb $ ppr $ "  * Core: Simplify\n"
	cSimplified	<- if elem Arg.OptSimplify ?args
				then SC.coreSimplify "CI" topVars cPrim cHeader
				else return cPrim
	

	-- Check the program one last time ------------------------------------
	--	Lambda lifting doesn't currently preserve the typing, 
	--	So we can't check it again after this point
	outVerb $ ppr $ "  * Core: Check\n"
	cProg_checked	
		<- SC.coreReconstruct  
			"core-reconstruct-final" 
			cHeader 
			cSimplified

	-- Convert to glob form.
	-- TODO: Use Glob for all of the core stages.
--	let cgProg_checked	= C.globOfTree cProg_checked
	let cgHeader		= C.globOfTree cHeader
	
	-- Slurp out ctor defs
	let mapCtorDefs	= Map.union
				(C.slurpCtorDefs cProg_checked)
				(C.slurpCtorDefs cHeader)
			
			
	-- Perform lambda lifting ---------------------------------------------
	outVerb $ ppr $ "  * Core: LambdaLift\n"
	(  cLambdaLift
	 , vsLambda_new) <- SC.coreLambdaLift
				cProg_checked
				cHeader


	-- Convert field labels to field indicies -----------------------------
	outVerb $ ppr $ "  * Core: LabelIndex\n"
	cLabelIndex	<- SC.coreLabelIndex
				mapCtorDefs
				cLambdaLift

	let cgProg_labelIndex	= C.globOfTree cLabelIndex

									
	-- Resolve partial applications ---------------------------------------
	outVerb $ ppr $ "  * Core: Curry\n"
	cCurry		<- SC.curryCall
				cLabelIndex
				cHeader
				cgProg_labelIndex	-- for super arities.
				cgHeader		-- TODO: refactor to just take the globs.

	let cgProg_final = C.globOfTree cCurry


	-- Generate the module interface --------------------------------------
	outVerb $ ppr $ "  * Make Interface File\n"
	diInterface	<- M.makeInterface
				modName
				sRenamed
				dProject
				(C.treeOfGlob cgProg_final)
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
				cgProg_final

	-- put blank lines after these sections,
	--	just to make the interface file look nicer.
	let sNewLineTags
		= Set.fromList ["ddc-version", "module", "imported-modules", "ctor"]
	
	when (elem Arg.DumpNewInterfaces ?args)
	 $ do writeFile (?pathSourceBase ++ ".di-new") 
		$ pprStrPlain $ pprDocIndentedWithNewLines sNewLineTags $ doc diNewInterface


	-- !! Early exit on StopCore
	when (elem Arg.StopCore ?args)
		compileExit

	-- Convert to Sea code ------------------------------------------------
	outVerb $ ppr $ "  * Convert to Sea IR\n"

	(eSea, eHeader)	<- SC.toSea
				"TE"
				cgHeader
				cgProg_final
				
	------------------------------------------------------------------------
	-- Sea stages
	------------------------------------------------------------------------
		
	-- Subsitute simple v1 = v2 statements ---------------------------------
	outVerb $ ppr $ "  * Sea: Substitute\n"
	eSub		<- SE.seaSub
				eSea
				
	-- Expand out constructors ---------------------------------------------
	outVerb $ ppr $ "  * Sea: ExpandCtors\n"
	eCtor		<- SE.seaCtor
				eSub
				
	-- Expand out thunking -------------------------------------------------
	outVerb $ ppr $ "  * Sea: Thunking\n"
	eThunking	<- SE.seaThunking
				eCtor
				
	-- Add suspension forcing code -----------------------------------------
	outVerb $ ppr $ "  * Sea: Forcing\n"
	eForce		<- SE.seaForce
				eThunking
				
	-- Add GC slots and fixup calls to CAFS --------------------------------
	outVerb $ ppr $ "  * Sea: Slotify\n"
	eSlot		<- SE.seaSlot	
				eForce
				eHeader
				cgHeader
				cgProg_final

	-- Flatten out match stmts --------------------------------------------
	outVerb $ ppr $ "  * Sea: Flatten\n"
	eFlatten	<- SE.seaFlatten
				"EF"
				eSlot

	-- Generate module initialisation functions ---------------------------
	outVerb $ ppr $ "  * Sea: Init\n"
	eInit		<- SE.seaInit
				modName
				eFlatten

	-- Generate C source code ---------------------------------------------
	outVerb $ ppr $ "  * Generate C source code\n"
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
				
	-- Write C files ------------------------------------------------------
	outVerb $ ppr $ "  * Write C files\n"
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

	 	 
