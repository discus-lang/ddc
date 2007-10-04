
module Main.Compile
(
	compileFile,
)

where

-----
import qualified System.IO		as System
import qualified System.Posix		as System
import qualified System.Cmd		as System
import qualified System.Exit		as System
import System.Time

import qualified Control.Exception	as Exception

import qualified Data.Map		as Map
import qualified Data.Set		as Set

import GHC.IOBase

-----
import qualified Shared.Var		as Var
import Shared.Var			(Module(..))

import qualified Main.Arg		as Arg
import Main.Arg 			(Arg)

import qualified Module.Graph		as M
import qualified Module.Export		as ME
import Module.IO			(munchFileName, chopOffExt)

import Stages.IO			as SI
import Stages.Source			as SS
import Stages.Core			as SC
import Stages.Sea			as SE
import Stages.Dump			as SD

import Source.Slurp			as S

import qualified Source.Pragma		as Pragma

import Main.Path

import qualified Core.Util.Slurp	as C

import qualified Desugar.Plate.Trans	as D
import qualified Sea.Util		as E

import Util
import Numeric

-----
-----
-- compileFile
--	Top level compilation function.
--	Compiles one source file.
--
--	Returns a list of module names and the paths to their compiled object files.
--	
--
compileFile ::	[Arg] -> String -> IO ()
compileFile	args     fileName
 = do 
	let  ?verbose	= or $ map (== Arg.Verbose) args

	when ?verbose 
	 (do
	 	putStr $ "\n* Compiling " ++ fileName ++ "\n")
	

	------------------------------------------------------------------------
	-- IO Stages
	------------------------------------------------------------------------

	-- Break out the file name
 	let Just (fileDir, fileBase, fileExt)
			= munchFileName fileName

	let Just pathBase	= chopOffExt fileName

	-- Gather up all the import dirs.
	let importDirs	= concat
			$ map (\a -> case a of { Arg.ImportDirs dirs -> dirs; _ -> []})
			$ args
	
	-- Choose a name for this module.
	let Just moduleName	
			= chooseModuleName 
				importDirs
				fileName
	when ?verbose
	 $  	putStr	$  "  - fileName      = " ++ fileName		++ "\n"
			++ "  - fileDir       = " ++ fileDir		++ "\n"
			++ "  - fileBase      = " ++ fileBase		++ "\n"
	 		++ "  - moduleName    = " ++ pretty moduleName	++ "\n"
			++ "\n"
		
	let filePaths	= makePaths (fileDir ++ "/" ++ fileBase)
	let  ?args	= Arg.ArgPath filePaths : args

	-- Load the source file
	sSource		<- readFile fileName

	-- Parse the source file.
	sParsed		<- SS.parse 
				fileName
				sSource

	-- Slurp out pragmas
	(  pragmaShell
	 , pragmaLinkObj)
	 		<- SS.sourcePragma sParsed

	-- Run any shell commands
	mapM_ (runPragmaShell fileName fileDir) pragmaShell

	-- Check if we're supposed to import the prelude
	let importPrelude	
			= not $ elem Arg.NoImplicitPrelude ?args

	-- Slurp out the list of modules imported by the source file
	let importsRoot	= S.slurpImportModules
				sParsed
			++ if importPrelude
				then [ModuleAbsolute ["Prelude"]]
				else []
	when ?verbose
	 $ 	putStr $ "  - importsRoot    = " ++ pretty importsRoot ++ "\n"


	-- Chase down imported modules
	importsExp	<- SI.chaseModules
				importDirs
				importsRoot
	
	let interfacePaths
		= [SI.idFilePathDI e | e <- Map.elems importsExp]

	
	-- Dump module hierarchy graph
	when ?verbose
	 $ do	putStr	$ "  * Imported modules\n"
	 	mapM_ dumpImportDef $ Map.elems importsExp
		putStr	$ "\n"

	
	let modulesCut	= concat
			$ [ss | Arg.GraphModulesCut ss <- ?args]
			
	SD.dumpDot Arg.GraphModules
		"modules"
		(M.dotModuleHierarchy 
			moduleName
			modulesCut 
			importsRoot
			importsExp)


	-- Chase down extra objects to link
	let linkObjsHere	= Pragma.slurpLinkObjs sParsed
	linkObjHerePaths
		<- SI.chaseLinkObjs
			[fileDir]
			linkObjsHere
				
	when ?verbose
	 $ do	mapM_ (\path -> putStr $ pretty $ "  - imported object " % path % "\n")
	 		$ linkObjHerePaths

	-- Chase down extra header files to include into source
	let includeFilesHere	= Pragma.slurpInclude sParsed
	
	when ?verbose
	 $ do	mapM_ (\path -> putStr $ pretty $ "  - included file   " % path % "\n")
	 		$ includeFilesHere
			
	 	
	------------------------------------------------------------------------
	-- Source Stages
	------------------------------------------------------------------------

	-- slurp out name of vars to inline
	inlineVars	<- SS.sourceSlurpInlineVars sParsed


	
	-- Rename variables and add uniqueBinds.
	((_, sRenamed) : modHeaderRenamedTs)
			<- SS.rename
				$ (moduleName, sParsed) 
				:[ (SI.idModule int, SI.idInterface int)
					| int <- Map.elems importsExp ]
	
	let hRenamed	= concat 
			$ [tree	| (mod, tree) <- modHeaderRenamedTs ]
			
	-- Slurp out header information and fixity defs.
	fixTable	<- SS.sourceSlurpFixTable
				(sRenamed ++ hRenamed)
	
	-- Defix the source.
	sDefixed	<- SS.defix
				sRenamed
				fixTable


	-- Slurp out kind table
	hKinds		<- SS.sourceKinds (sDefixed ++ hRenamed)
	
	-- Rename aliased instance functions
	--	and change main() to traumaMain()
	sAliased	<- SS.alias sDefixed

	-- Desugar the source language
	(sDesugared, hDesugared)
			<- SS.desugar
				hKinds
				sAliased
				hRenamed

	-- Snip down lambda expressions used as args to functions
{-	sSnipLambda	<- SS.desugarSnipLambda
				"desugared-snipLambda"
				"DSL"
				sDesugared
-}
	-- Snip down dictionaries and add default projections.
	(sProject, projTable)	
			<- SS.desugarProject
				hDesugared
				sDesugared
				

	------------------------------------------------------------------------
	-- Type stages
	------------------------------------------------------------------------

	-- Slurp out type constraints.
	when ?verbose
	 $ do	putStr	$ "  * Slurping type constraints.\n"

	(  sTagged
	 , sConstrs
	 , sigmaTable
	 , vsTypesPlease)
			<- SS.slurpC 
				sProject
				hDesugared

	let hTagged	= map (D.transformN (\n -> Nothing)) hDesugared

	-- !! Early exit on StopConstraint
	when (elem Arg.StopConstraint ?args)
		compileExit

	when ?verbose
	 $ do	putStr $ "  * Solving type constraints.\n"

	-- Solve type constraints
	(  typeTable
	 , typeInst)
	 		<- runStage "solve"
			$  SS.solveSquid
				sConstrs
				vsTypesPlease
				sigmaTable

	-- !! Early exit on StopType
	when (elem Arg.StopType ?args)
		compileExit

		
	-- Convert source tree to core tree
	(  cSource
	 , cHeader )	<- runStage "core"
	 		$ SS.toCore
		 		sTagged
				hTagged
				sigmaTable
				typeTable
				typeInst
				projTable

	-- Slurp out the list of all the vars defined at top level.
	let topVars	= Set.union 
				(Set.fromList $ concat $ map C.slurpBoundVarsP cSource)
				(Set.fromList $ concat $ map C.slurpBoundVarsP cHeader)	

	------------------------------------------------------------------------
	-- Core stages
	------------------------------------------------------------------------	

	-- Convert to normal form
	cNormalise	<- SC.coreNormalise "core-normalise" "CN" topVars cSource
				
	-- Create local regions.
	cBind		<- SC.coreBind "core-bind" "CB"
				(\x -> Just [])
				cNormalise

	-- From this point on all the vars should be bound and all the required
	--	type information should be present. 
	--
	-- The tree should be lint free from this point on.
	
	SC.coreLint cBind cHeader


	-- Reconstruct type annotations on bindings.
--	cReconstruct	<- runStage "reconstruct"
--			$ SC.coreReconstruct "core-reconstruct" cHeader cCrush
	
	-- Call class instance functions and add dictionaries.
	cDict		<- SC.coreDict 
				cHeader
				cBind

	-- Mask out effects on const regions
--	cMaskConst	<- SC.coreMaskEffs cBind



--	runStage "lint-reconstruct" 
--		$ SC.coreLint cReconstruct cHeader

	-- Identify primitive operations
	cPrim		<- SC.corePrim	cDict

	-----------------------
	-- Optimisations

	-- Inline forced inline functions
	cInline		<- SC.coreInline
				cPrim
				cHeader
				inlineVars

	-- Perform local unboxing.
	cBoxing		<- if elem Arg.OptBoxing ?args
				then SC.coreBoxing topVars cInline cHeader
				else return cInline
	
	-- Do the full laziness optimisation.
	cFullLaziness	<- SC.coreFullLaziness 
				moduleName
				cBoxing 
				cHeader



	----------------------
	-- 

	-- Snip the tree again to make sure
	--	all the appropriate function calls are exposed as their own bindings.
--	cSnip2		<- SC.coreSnip "core-snip2" "CSb" topVars cFullLaziness

	-- Reconstruct type annotations on bindings.
	cReconstruct2	<- runStage "reconstruct2"
			$  SC.coreReconstruct  "core-reconstruct2" cHeader cFullLaziness

	-- Perform lambda lifting.
	(  cLambdaLift
	 , vsLambda_new) <- SC.coreLambdaLift
				cReconstruct2
				cHeader

	runStage "lint-lifted" 
		$ SC.coreLint cLambdaLift cHeader

	
	-- Slurp out ctor defs
	let mapCtorDefs	= Map.union
				(C.slurpCtorDefs cLambdaLift)
				(C.slurpCtorDefs cHeader)
				
	-- Convert field labels to field indicies
	cLabelIndex	<- SC.coreLabelIndex
				mapCtorDefs
				cLambdaLift
				
	-- Sequence bindings and CAFs
	cSequence	<- SC.coreSequence	
				cLabelIndex
				cHeader
				
				
	-- Resolve partial applications.
	(cCurry, cafVars)
			<- SC.curryCall
				cSequence
				cHeader

	-- Rewrite so atoms are shared.
	cAtomise	<- if elem Arg.OptAtomise ?args
				then SC.coreAtomise cCurry cHeader
				else return cCurry
	
	-- Generate the module interface
	--
	diInterface	<- ME.makeInterface
				moduleName
				sRenamed
				sProject
				cAtomise
				sigmaTable
				typeTable
				vsLambda_new

	writeFile (pathDI filePaths)	diInterface	

	-- Ditch type information in preparation for conversion to Sea
	cDitch		<- SC.coreDitch 
				cAtomise


	-- !! Early exit on StopCore
	when (elem Arg.StopCore ?args)
		compileExit

	-- Convert to Sea code.
	(eSea, eHeader)	<- SC.toSea
				cDitch
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
				cafVars

	-- Flatten out match stmts.
	eFlatten	<- SE.seaFlatten
				eSlot

	-- Generate module initialisation functions.
	eInit		<- SE.seaInit
				moduleName
				eFlatten

	-- Generate C source code
	(  seaHeader
	 , seaSource )	<- SE.outSea
				moduleName
	 			eInit
				fileName
				interfacePaths
				includeFilesHere

	-- If this module binds the top level main function
	--	then append RTS initialisation code.
	--
	seaSourceInit	<- if SE.gotMain eInit 
				then do mainCode <- SE.seaMain $ map fst $ Map.toList importsExp
				     	return 	$ seaSource ++ (catInt "\n" $ map pretty $ E.eraseAnnotsTree mainCode)
				else 	return  $ seaSource
				
	writeFile (pathC filePaths)	seaSourceInit
	writeFile (pathH filePaths)	seaHeader


	------------------------------------------------------------------------
	-- Invoke external compiler / linker
	------------------------------------------------------------------------

	-- !! Early exit on StopSea
	when (elem Arg.StopSea ?args)
		compileExit	
	

	-- Invoke GCC to compile C source.
	SE.invokeSeaCompiler


	-- !! Early exit on StopCompile
	when (elem Arg.StopCompile ?args)
		compileExit
			
	
	-- Invoke GCC to link main binary.
	SE.invokeLinker
		([pathBase ++ ".o"]
			++ (map    idFilePathO $ Map.elems importsExp)
			++ (catMap idLinkObjs  $ Map.elems importsExp))	

	return ()
		
-----
compileExit
 = do
 	System.exitWith System.ExitSuccess



runStage :: String -> IO a -> IO a
runStage name stage
 =	Exception.catch stage (handleStage name)
		
 
handleStage :: String -> Exception -> IO a
handleStage name ex
 = case ex of
 	ErrorCall string	
	 -> do	putStr $ "DDC: error in stage " ++ name ++ "\n"
	 	putStr $ string ++ "\n"
		
		error "failed."
		
	_ ->	throwIO ex





putStrF s
 = do
 	System.hFlush System.stdout
	putStr s
 	System.hFlush System.stdout
	

getTimeF :: IO Double
getTimeF 
 = do
 	clock		<- getClockTime
	calTime		<- toCalendarTime clock
	
	let secs	= (ctMin calTime) * 60
			+ (ctSec calTime)
			
	let frac 	= ((fromIntegral $ ctPicosec calTime) / 1000000000000.0 :: Double)
			
	return (fromIntegral secs + frac)
		
showF :: Double -> String
showF	f	= (showFFloat (Just 6) f "") ++ "s"



-----
runPragmaShell fileName fileDir c
 = do
	when ?verbose
	 $ 	putStr $ pretty $ "  - pragma Shell \"" % c % "\"\n"

	-- Change to the same dir as the source file.
	oldDir	<- System.getWorkingDirectory
	System.changeWorkingDirectory fileDir	 

	-- Run the command.
	ret	<- System.system c

	-- Change back to original dir.
	System.changeWorkingDirectory oldDir
	
	case ret of
	 System.ExitSuccess	-> return ()
	 System.ExitFailure _
	  -> do	System.hPutStr System.stderr 
		  	$  pretty
			$ "* DDC ERROR - embedded pragma shell command failed.\n"
			% "    source file = '" % fileName % "'\n"
			% "    command     = '" % c % "'\n"
			% "\n"
	
		System.exitWith (System.ExitFailure 1)
	 	 
-----
dumpImportDef def
 	| isNil (idLinkObjs def)
	= putStr	
	$ pretty
 	$ "        " % (padR 40 $ pretty $ idModule def) % " " % idFilePathDI def % "\n"
 
 | otherwise
 = putStr
 	$ pretty
 	$ "        " % (padR 40 $ pretty $ idModule def) % " " % idFilePathDI def % "\n"
	% "          linkObjs:\n" 
	% "\n" %!% (map (\p -> "             " % p) $ idLinkObjs def)
	% "\n"
	




