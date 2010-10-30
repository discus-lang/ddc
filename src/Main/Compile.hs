
module Main.Compile
	(compileFile)
where

-- This is the module which ties the whole compilation pipeline together
--	so there'll be a lot of imports...

-- main stages
import Main.Setup
import Main.Error
import Main.Util
import Main.Sea
import Main.Llvm
import qualified Main.Dump		as Dump
import qualified Main.Source		as SS
import qualified Main.Desugar		as SD
import qualified Main.Core		as SC
import qualified DDC.Main.Arg		as Arg

-- module
import qualified Module.Interface.Docable ()
import qualified Module.Export		as M
import qualified Module.Interface.Export as MN
import qualified DDC.Module.Scrape	as M

-- source
import qualified Source.Exp		as S
import qualified Source.Pragma		as Pragma

-- desugar
import qualified DDC.Desugar.Transform	as D
import qualified DDC.Desugar.Glob	as D
import qualified DDC.Solve.Interface.Problem as T
import qualified Type.Export		as T

-- core
import qualified DDC.Core.Glob		as C

-- shared
import DDC.Var
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Util.Doc
import DDC.Util.FreeVars

-- haskell
import Util
import System.FilePath
import System.Directory
import qualified Data.Map		as Map
import qualified Data.Set		as Set


out ss		= putStr $ pprStrPlain ss

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
	let filePath	= let Just s = M.scrapePathSource sRoot in s
	let fileDir	= takeDirectory filePath

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
	sSource		<- readFile filePath

	compileFile_parse 
		setup scrapes sRoot sModule
		importDirs blessMain  sSource


compileFile_parse
	setup scrapes sRoot sModule
	importDirs blessMain sSource

 -- Emit a nice error message if the source file is empty.
 --	If we parse an empty file to happy it will bail with "reading EOF!" which isn't as nice.
 | words sSource == []
 = do	let Just pathSource	= M.scrapePathSource sRoot
	dirWorking		<- getCurrentDirectory
	let pathRelative	=  "./" ++ makeRelative dirWorking pathSource
	
   	exitWithUserError (setupArgs setup) 
   		[pathRelative % "\n    Source file is empty.\n"]

 | otherwise
 = do	let ?args		= setupArgs setup
	let ?verbose		= elem Arg.Verbose ?args

	-- extract some file names
	let modName		= M.scrapeModuleName sRoot
	let pathSource		= let Just s = M.scrapePathSource sRoot in s

	let fileDir		= takeDirectory pathSource
	let fileBase		= takeBaseName  pathSource
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


	-- Parse the source file ----------------------------------------------
	outVerb $ ppr $ "  * Source: Parse\n"
	(sParsed, pragmas)	
			<- {-# SCC "Main.parsed" #-} SS.parse pathSource sSource

	-- Slurp out pragmas
	let pragmas	= Pragma.slurpPragmaTree sParsed

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

	(sHeader_linted, sProg_linted)	
			<- SS.lint 
				hRenamed 
				sDefixed
	
	-- Desugar the source language ----------------------------------------
	outVerb $ ppr $ "  * Convert to Desugared IR\n"
	(hDesugared, sDesugared)
			<- {-# SCC "Main.desugared" #-}
			   SS.desugar
				"SD"
				kindTable
				sHeader_linted
				sProg_linted

	-----------------------------------------------------------------------
	-- Desugar/Type inference stage
	-----------------------------------------------------------------------
	
	let dgHeader_desugared	= D.globOfTree hDesugared
	let dgModule_desugared	= D.globOfTree sDesugared
	
	-- Elaborate type information in data defs and signatures -------------
	outVerb $ ppr $ "  * Desugar: Elaborate\n"
	(dgHeader_elab, dgModule_elab, kindTable)	
			<- SD.desugarElaborate 
				"DE" 
				dgHeader_desugared
				dgModule_desugared

	let hElab	= D.treeOfGlob dgHeader_elab
	let sElab	= D.treeOfGlob dgModule_elab

	-- Eta expand simple v1 = v2 projections ------------------------------
	outVerb $ ppr $ "  * Desugar: ProjectEta\n"
	sProjectEta	<- SD.desugarProjectEta
				"DE" 
				sElab
			
	-- Snip down dictionaries and add default projections -----------------
	outVerb $ ppr $ "  * Desugar: Project\n"
	(dProg_project, projTable)	
			<- SD.desugarProject
				"SP" 
				modName
				hElab
				sProjectEta

	-- Slurp out type constraints -----------------------------------------
	outVerb $ ppr $ "  * Desugar: Slurp\n"

	(sTagged, problem)
			<- SD.desugarSlurpConstraints
				blessMain
				dProg_project
				hElab

	let hTagged	= map (D.transformN (\n -> Nothing)) hElab

	-- !! Early exit on StopConstraint
	when (elem Arg.StopConstraint ?args)
		compileExit

	-- Solve type constraints ---------------------------------------------
	outVerb $ ppr "  * Type: Solve\n"
	solution	<- SD.desugarSolveConstraints problem

	-- !! Early exit on StopType
	when (elem Arg.StopType ?args)
		compileExit

	-- Convert source tree to core tree -----------------------------------
	outVerb $ ppr $ "  * Convert to Core IR\n"

	(  cSource
	 , cHeader )	<- SD.desugarToCore
		 		sTagged
				hTagged
				(T.problemValueToTypeVars problem)
				projTable
				solution 
				
	let cgHeader	= C.globOfTree cHeader
	let cgModule	= C.globOfTree cSource

	------------------------------------------------------------------------
	-- Core stages
	------------------------------------------------------------------------	

	-- Convert to normalised form -----------------------------------------
	outVerb $ ppr $ "  * Core: NormalDo\n"
	cgModule_normal	<- SC.coreNormaliseDo 
				"core-normaldo"
				"CN" 
				cgModule
							
	-- Create local regions -----------------------------------------------
	outVerb $ ppr $ "  * Core: Bind\n"

	-- These are the region vars that are free in the type of a top level binding
	let rsGlobal	= Set.filter (\v -> varNameSpace v == NameRegion)
			$ Set.unions
			$ map freeVars
			$ [t	| (v, t)	<- Map.toList $ T.solutionTypes solution
				, Set.member v (T.problemTopLevelTypeVars problem)]
		
	cgModule_bind	<- SC.coreBind 
				sModule 
				"CB"
				(T.solutionRegionClasses solution)
				rsGlobal
				cgModule_normal

	-- Convert to A-normal form -------------------------------------------
	outVerb $ ppr $ "  * Core: Snip\n"
	cgModule_snip	<- SC.coreSnip 
				"core-snip" 
				"CS" 
				cgHeader 
				cgModule_bind

	-- Thread through witnesses -------------------------------------------
	outVerb $ ppr $ "  * Core: Thread\n"
	cgModule_thread	<- SC.coreThread 
				cgHeader 
				cgModule_snip

	-- Reconstruct and check types (with the linter) ----------------------
	outVerb $ ppr $ "  * Core: Lint Reconstruct\n"
	cgModule_lintRecon	
			<- SC.coreLint 
				"core-lint-reconstruct" 
				cgHeader
				cgModule_thread

	-- Rewrite projections to use instances from dictionaries -------------
	outVerb $ ppr $ "  * Core: Dict\n"
	cgModule_dict	<- SC.coreDictionary 
				cgHeader
				cgModule_lintRecon

	-- Identify prim ops --------------------------------------------------
	outVerb $ ppr $ "  * Core: Prim\n"
	cgModule_prim	<- SC.corePrim
				cgHeader
				cgModule_dict

	-- Simplifier does various simple optimising transforms ---------------
	outVerb $ ppr $ "  * Core: Simplify\n"
	cgModule_simplified	
			<- if elem Arg.OptSimplify ?args
				then SC.coreSimplify "CI" cgHeader cgModule_prim
				else return cgModule_prim
					
	-- Perform lambda lifting ---------------------------------------------
	outVerb $ ppr $ "  * Core: LambdaLift\n"
	(  cgModule_lambdaLifted
	 , vsNewLambdaLifted) 
			<- SC.coreLambdaLift
				cgHeader
				cgModule_simplified

	-- Check the program one last time ------------------------------------
	--	Lambda lifting doesn't currently preserve the typing, so we can't
	--	check it again after this point. This panics if there is any lint.
	outVerb $ ppr $ "  * Core: Lint (final)\n"
	cgModule_lintFinal  
			<- SC.coreLint
				"core-lint-final" 
				cgHeader 
				cgModule_lambdaLifted


	-- Convert field labels to field indicies -----------------------------
	outVerb $ ppr $ "  * Core: LabelIndex\n"
	cgModule_labelIndex
			<- SC.coreLabelIndex
				cgHeader
				cgModule_lintFinal
									
	-- Resolve partial applications ---------------------------------------
	outVerb $ ppr $ "  * Core: Curry\n"
	cgCurry		<- SC.coreCurryCall
				cgHeader
				cgModule_labelIndex

	let cgModule_final = cgCurry

	-- Generate the module interface --------------------------------------
	outVerb $ ppr $ "  * Make interface file\n"
	
	-- Don't export types of top level bindings that were created during lambda lifting.
	let vsNoExport	= vsNewLambdaLifted

	diInterface	<- M.makeInterface
				modName
				sRenamed
				dProg_project
				(C.treeOfGlob cgModule_final)
				(T.problemValueToTypeVars problem)
				(T.solutionTypes solution)
				vsNoExport

	writeFile (?pathSourceBase ++ ".di") diInterface	

	-- Make the new style module interface.
	outVerb $ ppr $ "  * Make new style interface file\n"
	let Just thisScrape	
		= Map.lookup modName scrapes

	let diNewInterface	
	 		= MN.makeInterface
				thisScrape
				vsNoExport
				(T.problemValueToTypeVars problem)
				(T.solutionTypes solution)
				sProg_linted
				dProg_project
				cgModule_final

	when (elem Arg.DumpNewInterfaces ?args)
	 $ do writeFile (?pathSourceBase ++ ".di-new") 
		$ pprStrPlain $ pprDocIndentedWithNewLines 2 $ doc diNewInterface

	-- !! Early exit on StopCore
	when (elem Arg.StopCore ?args)
		compileExit



	-- Chase down extra C header files to include into source -------------
	let includeFilesHere	= [ str | Pragma.PragmaCCInclude str <- pragmas]
	
	when (elem Arg.Verbose ?args)
	 $ do	mapM_ (\path -> putStr $ pprStrPlain $ "  - included file   " % path % "\n")
	 		$ includeFilesHere

	let cgModule	= cgModule_final

	-- Convert to Sea code ------------------------------------------------
	outVerb $ ppr $ "  * Convert to Sea IR\n"

	(eSea, eHeader)	<- SC.toSea
				"TE"
				cgHeader
				cgModule

	------------------------------------------------------------------------
	-- Sea stages
	------------------------------------------------------------------------
		
	-- Subsitute simple v1 = v2 statements ---------------------------------
	outVerb $ ppr $ "  * Sea: Substitute\n"
	eSub		<- seaSub
				eSea
				

	-- Expand out constructors ---------------------------------------------
	outVerb $ ppr $ "  * Sea: ExpandCtors\n"
	eCtor		<- seaCtor
				eSub
				
	-- Expand out thunking -------------------------------------------------
	outVerb $ ppr $ "  * Sea: Thunking\n"
	eThunking	<- seaThunking
				eCtor
				
	-- Add suspension forcing code -----------------------------------------
	outVerb $ ppr $ "  * Sea: Forcing\n"
	eForce		<- seaForce
				eThunking
				
	-- Add GC slots and fixup calls to CAFS --------------------------------
	outVerb $ ppr $ "  * Sea: Slotify\n"
	eSlot		<- seaSlot	
				eForce
				eHeader
				cgHeader
				cgModule

	-- Flatten out match stmts --------------------------------------------
	outVerb $ ppr $ "  * Sea: Flatten\n"
	eFlatten	<- seaFlatten
				"EF"
				eSlot

	-- Generate module initialisation functions ---------------------------
	outVerb $ ppr $ "  * Sea: Init\n"
	eInit		<- seaInit
				modName
				eFlatten

	-- TODO : Put the parameters into a struct and call compileViaX with just
	-- a single parameter.
	if elem Arg.ViaLLVM ?args
	  then compileViaLlvm
		setup modName eInit pathSource importDirs includeFilesHere importsExp
		modDefinesMainFn sRoot scrapes_noRoot blessMain
	  else compileViaSea
		setup modName eInit pathSource importDirs includeFilesHere importsExp
		modDefinesMainFn sRoot scrapes_noRoot blessMain

