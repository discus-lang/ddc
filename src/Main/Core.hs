{-# OPTIONS -fwarn-unused-imports #-}
{-# OPTIONS -XNoMonomorphismRestriction #-}

-- | Wrappers for the compiler stages dealing with the Core IR.
--	These wrappers are responsible for calling the functions that actually
--	implement the transforms, and for dumping debugging info.
--
module Main.Core
	( coreTidy
	, coreBind
	, coreSnip
	, coreThread
	, coreDictionary
	, corePrim
	, coreSimplify
	, coreLint
	, coreLambdaLift
	, corePrep
	, coreCurry
	, coreToSea)
where
import Main.Dump
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Main.Arg
import DDC.Core.Glob
import DDC.Core.Check			(checkGlobs)
import DDC.Var
import Core.Block			(blockP)
import Core.Dictionary			(dictGlob)
import Core.Bind			(bindGlob)
import Core.Thread			(threadGlob)
import Core.Prim			(primGlob)
import DDC.Core.Simplify
import Core.Lift			(lambdaLiftGlob)
import Core.LabelIndex			(labelIndexGlob)
import Core.Curry			(curryGlob)
import DDC.Core.ToSea			(toSeaGlobs)
import DDC.Core.Exp
import Core.Plate.Trans
import Data.Foldable			(foldr)
import Util				hiding (foldr)
import Prelude				hiding (foldr)
import qualified Core.Snip		as Snip
import qualified DDC.Sea.Exp		as E
import qualified Sea.Util		as E


-- | A consistent interface for Core Stages.
--   TODO: They're all pretty regular, we should refactor them to match this type.
type CoreStage
	=  String		-- ^ Name of this stage to use for dump files.
	-> [Arg] 		-- ^ Cmd line args of compiler.
	-> FilePath		-- ^ Base name of path for dump files.
	-> String		-- ^ Unique id for generating fresh names.
	-> Glob			-- ^ Header glob
	-> Glob			-- ^ Core glob
	-> IO Glob


---------------------------------------------------------------------------------------------------
-- | Tidy up after conversion to core.
--   The Desugar -> Core conversion doesn't produce very nice code.
coreTidy :: CoreStage
coreTidy stage args base unique cgHeader cgModule
 = {-# SCC "Core/tidy" #-}
   do	let cgModule_tidy	= fst $ simplifyPassTidy unique cgHeader cgModule
 	dumpCG args base DumpCoreTidy stage cgModule_tidy
	return	cgModule_tidy


---------------------------------------------------------------------------------------------------
-- | Bind local regions.
coreBind
	:: ModuleId
	-> (Map Var [Var])	-- ^ TODO: get this from the glob.
				--         map of class constraints on each region eg (%r1, [Lazy, Const]).
	-> Set Var		-- ^ TODO: get this from the glob: he regions with global lifetimes
				--         which should be bound at top level.
	-> CoreStage

coreBind modId classMap rsGlobal
	 stage args base unique cgHeader cgModule
 = {-# SCC "Core/bind" #-}
   do	let cgModule'
		= bindGlob modId unique classMap rsGlobal
		$ mapBindsOfGlob blockP cgModule

	dumpCG args base DumpCoreBind stage cgModule'
	return cgModule'


---------------------------------------------------------------------------------------------------
-- | Convert to A-Normal form.
coreSnip
	:: (?args	:: [Arg])
	=> (?pathSourceBase :: FilePath)
	=> String 		-- ^ stage name
	-> String 		-- ^ unique
	-> Glob			-- ^ Header glob.
	-> Glob			-- ^ Module glob.
	-> IO Glob

coreSnip stage unique cgHeader cgModule
 = {-# SCC "Core/snip" #-}
   do	-- snip exprs out of fn arguments
	let snipTable	= Snip.Table
			{ Snip.tableHeaderGlob		= cgHeader
			, Snip.tableModuleGlob		= cgModule
			, Snip.tablePreserveTypes	= False }

	let cgModule'	= Snip.snipGlob snipTable ("x" ++ unique) cgModule

	dumpCT DumpCoreSnip (stage ++ "-snip")
		$ treeOfGlob cgModule'

	return cgModule'



---------------------------------------------------------------------------------------------------
-- | Resolve calls to overloaded functions.
coreDictionary
	:: (?args	:: [Arg])
	=> (?pathSourceBase :: FilePath)
	=> Glob 		-- ^ Header glob.
	-> Glob			-- ^ Module glob.
	-> IO Glob

coreDictionary cgHeader cgModule
 = {-# SCC "Core/dict" #-}
   do	let cgModule'	= dictGlob cgHeader cgModule

 	dumpCT DumpCoreDict "core-dict"
		$ treeOfGlob cgModule'

	return	cgModule'


---------------------------------------------------------------------------------------------------
-- | Thread through witness variables.
coreThread
	:: (?args :: [Arg])
	=> (?pathSourceBase :: FilePath)
	=> Glob 		-- ^ Header Glob.
	-> Glob 		-- ^ Module Glob.
	-> IO Glob

coreThread cgHeader cgModule
 = {-# SCC "Core/thread" #-}
   do	let cgModule' = threadGlob cgHeader cgModule

 	dumpCT DumpCoreThread "core-thread"
		$ treeOfGlob cgModule'

	return cgModule'


---------------------------------------------------------------------------------------------------
-- | Identify primitive operations.
corePrim
	:: (?args ::	[Arg])
	=> (?pathSourceBase :: FilePath)
	=> Glob			-- ^ Header glob.
	-> Glob			-- ^ Module glob.
	-> IO Glob

corePrim cgHeader cgModule
 = {-# SCC "Core/prim" #-}
   do	let cgModule'	= primGlob cgModule

 	dumpCT DumpCorePrim "core-prim"
		$ treeOfGlob cgModule'

	return cgModule'


---------------------------------------------------------------------------------------------------
-- | Do core simplification
coreSimplify
	:: (?args :: [Arg])
	=> (?pathSourceBase :: FilePath)
	=> String -> Glob -> Glob -> IO Glob

coreSimplify unique cgHeader cgModule
 = {-# SCC "Core/simplify" #-}
   do	let (cgModule', statss)
 		= fixSimplifierPass simplifyPassAll unique cgHeader cgModule

	when (elem Verbose ?args)
	 $ do	putStr	$ pprStrPlain	$ "\n" %!% statss % "\n\n"

	when (elem DumpCoreSimplify ?args)
	 $ do	dumpCT DumpCoreSimplify "core-simplify" $ treeOfGlob cgModule'
		dumpS  DumpCoreSimplify "core-simplify--stats"
			$ pprStrPlain $ vcat $ map ppr statss

	return	cgModule'


---------------------------------------------------------------------------------------------------
-- | Check the tree for syntactic problems that won't be caught by type checking.
coreLint
	:: (?args :: 	[Arg])
	=> (?pathSourceBase :: FilePath)
	=> String		-- ^ stage name
	-> Glob 		-- ^ core tree
	-> Glob 		-- ^ header tree
	-> IO Glob

coreLint stage cgHeader cgModule
 = {-# SCC "Core/lint" #-}
   do	let cgModule'	= checkGlobs ("Compile.coreLint." ++ stage) cgHeader cgModule

	dumpCT DumpCoreLint stage
		$ treeOfGlob cgModule'

	return cgModule'


---------------------------------------------------------------------------------------------------
-- | Lift nested functions to top level.
coreLambdaLift
	:: (?args :: [Arg])
	=> (?pathSourceBase :: FilePath)
	=> Glob				-- ^ Header glob.
	-> Glob				-- ^ Module glob.
	-> IO	( Glob			-- transformed module glob, including new lifted bindings.
		, Set Var)		-- the vars of the new bindings.

coreLambdaLift cgHeader cgModule
 = {-# SCC "Core/lift" #-}
   do	let (cgModule', vsNewLambdaLifted)
		= lambdaLiftGlob cgHeader cgModule

	dumpCT DumpCoreLift "core-lift" 		$ treeOfGlob cgModule'
	dumpS  DumpCoreLift "core-lift--new-bindings"	$ show vsNewLambdaLifted

	return	( cgModule'
		, vsNewLambdaLifted)


---------------------------------------------------------------------------------------------------
-- | Prepare for conversion to Sea.
corePrep :: CoreStage
corePrep stage args base unique cgHeader cgModule
 = {-# SCC "Core/prep" #-}
   do	let eatXTau	= transformX
			$ \xx -> case xx of
					XTau _ x -> x
					_	 -> xx

	let cgModule2	= labelIndexGlob cgHeader cgModule
	let cgModule3	= mapBindsOfGlob (blockP . eatXTau) cgModule2

	-- snip exprs out of fn arguments
	let snipTable	= Snip.Table
			{ Snip.tableHeaderGlob		= cgHeader
			, Snip.tableModuleGlob		= cgModule
			, Snip.tablePreserveTypes	= True }

	let cgModule_snipped
			= Snip.snipGlob snipTable ("x" ++ unique) cgModule3

	dumpCG args base DumpCorePrep stage cgModule_snipped
	return	cgModule_snipped


---------------------------------------------------------------------------------------------------
-- | Identify partial applications and insert calls to explicitly create and apply thunks.
coreCurry
	:: (?args :: [Arg])
	=> (?pathSourceBase :: FilePath)
	=> Glob -> Glob -> IO Glob

coreCurry cgHeader cgModule
 = {-# SCC "Core/curry" #-}
   do	let optTailCall	= elem OptTailCall ?args
	let cgModule'	= curryGlob optTailCall cgHeader cgModule

	dumpCT DumpCoreCurry "core-curry" 		$ treeOfGlob cgModule'
	return	cgModule'


---------------------------------------------------------------------------------------------------
-- | Convert Core-IR to Abstract-C
coreToSea
	:: (?args :: [Arg])
	=> (?pathSourceBase :: FilePath)
	=> String			-- ^ Unique.
	-> Glob				-- ^ Header glob.
	-> Glob				-- ^ Module glob.
	-> IO 	( E.Tree ()		-- sea source tree
		, E.Tree ())		-- sea header tree

coreToSea unique cgHeader cgModule
 = {-# SCC "Core/toSea" #-}
   do	let result = toSeaGlobs unique cgHeader cgModule
	case result of
	 Left vsRecursive
	  -> exitWithUserError ?args
	 	$ ["Values may not be mutually recursive.\n"
		% "     offending variables: " % vsRecursive % "\n\n"]

	 Right (esHeader, esModule)
	  -> do
		let esHeader_list = foldr (:) [] esHeader
		dumpET DumpSea "sea--header" $ E.eraseAnnotsTree esHeader_list

		let esModule_list = foldr (:) [] esModule
		dumpET DumpSea "sea--source" $ E.eraseAnnotsTree esModule_list

 		return (esHeader_list, esModule_list)

