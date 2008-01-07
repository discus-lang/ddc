
-- | Wrappers for the compiler stages dealing with the Core IR
--	These wrappers are responsible for calling the functions that actually
--	implement the various transforms, and for dumping debugging info.
--
module Main.Core
	( coreClean
	, coreNormalise
	, coreDict
	, coreReconstruct
	, coreBind
	, coreThread
	, corePrim
	, coreBoxing
	, coreFullLaziness
	, coreInline
	, coreLint
	, coreLambdaLift
	, coreLabelIndex
	, coreSequence
	, curryCall
	, coreAtomise
	, coreDitch
	, toSea)

where

-- These are all the transforms, in rough order that they are applied to the core program.
--
import Core.Clean			(cleanTree)
import Core.Block			(blockTree)
import Core.Snip			(snipTree)
import Core.Crush			(crushTree)

import Core.Dictionary			(dictTree)
import Core.Reconstruct			(reconstructTree)
import Core.Bind			(bindTree)
import Core.Thread			(threadTree)
import Core.Prim			(primTree)
import Core.Lint			(lintTree)
import Core.Lift			(lambdaLiftTree)
import Core.LabelIndex			(labelIndexTree)
import Core.Curry			(curryTree, slurpSupersTree, isCafP_opType)
import Core.Ditch			(ditchTree)
import Core.ToSea			(toSeaTree)

import Core.Optimise.Boxing		(coreBoxingTree)
import Core.Optimise.Atomise		(atomiseTree)
import Core.Optimise.FullLaziness	(fullLazinessTree)
import Core.Optimise.Inline		(inlineTree)

------
import Core.Exp 	
import Core.Util

import Core.Graph
import Core.Plate.Util			(eraseModuleTree)
import Core.Sequence			(slurpSuperDepsTree, dotSuperDeps, sequenceCafsTree)

import qualified Sea.Exp	as E
import qualified Sea.Util	as E

import Main.Arg
import Main.Dump


import qualified Shared.Var	as Var
import Shared.Var		(Var, Module)
import Shared.Error

-----
import Util
import Debug.Trace

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)



-- | Clean out effect and closure variables which can never be 
--	anything but bottom.
coreClean
	:: (?args	:: [Arg])
	=> Tree 		-- ^ core tree
	-> IO Tree

coreClean tree
 = do	when (elem Verbose ?args)
	 $ do	putStr $ "  * Core: Clean\n"
	 
	let treeClean	= cleanTree tree
 	dumpCT DumpCoreClean "core-clean" treeClean
	
	return treeClean
 	
	

-- | Convert to A-Normal form.
coreNormalise
	:: (?args	:: [Arg])
	-> String 		-- ^ stage name
	-> String 		-- ^ unique
	-> Set Var 		-- ^ vars bound at top level
	-> Tree			-- ^ core tree
	-> IO Tree
	
coreNormalise stage unique topVars tree
 = do	
 	-- ensure all exprs are wrapped in do blocks.
 	let treeBlock	= blockTree tree
 	dumpCT DumpCoreBlock (stage ++ "-block") treeBlock

	-- snip exprs out of fn arguments
	let treeSnip	= snipTree topVars ("x" ++ unique) treeBlock
	dumpCT DumpCoreSnip (stage ++ "-snip")  treeSnip
	
	-- crush nested do exprs
	let treeCrush	= crushTree treeSnip
 	dumpCT DumpCoreCrush (stage ++ "-crush") treeCrush

	return treeCrush



-- | Resolve calls to overloaded functions.
coreDict
	:: (?args	:: [Arg])
	-> Tree 		-- ^ header tree
	-> Tree			-- ^ core tree
	-> IO Tree

coreDict hTree sTree
 = do	let tree'	= dictTree hTree sTree
 	dumpCT DumpCoreDict "core-dict" tree'
	return	tree'



-- | Reconstruct and check type information.
coreReconstruct
	:: (?args	:: [Arg])
	-> String		-- ^ stage name
	-> Tree			-- ^ header tree
	-> Tree			-- ^ core tree
	-> IO Tree
	
coreReconstruct name cHeader cTree
 = do	let cTree'	= {-# SCC "Core.Reconstruct" #-} 
 			   reconstructTree name cHeader cTree
 	dumpCT DumpCoreReconstruct name cTree'
	return	cTree'


	
-- | Bind local regions.
coreBind
	:: (?args ::	[Arg])
	-> String		-- ^ unique
	-> (Map Var [Var])	-- ^ map of class constraints on each region
				--	eg (%r1, [Lazy, Const])
	-> Set Var		-- the regions with global lifetimes which should be bound 
				--	at top level.
	-> Tree	-> IO Tree
	
coreBind
	unique	
	classMap
	rsGlobal
	cSource
 = do
 	let tree' = {-# SCC "Core.Bind" #-}
	            bindTree unique classMap rsGlobal cSource
	
	dumpCT DumpCoreBind "core-bind" tree'

	dumpS  DumpCoreBind "core-bind--rsGlobal" 
		$ catInt "\n"
		$ map pretty $ Set.toList rsGlobal
	
	return tree'



-- | Thread through witness variables.
coreThread
	:: (?args :: [Arg])
	=> Tree 		-- ^ header tree
	-> Tree 		-- ^ core tree
	-> IO Tree
	
coreThread hTree cTree
 = do	let tree'	= {-# SCC "Core.Thread " #-} 
 			  threadTree hTree cTree
 
 	dumpCT DumpCoreThread "core-thread" tree'
	return tree'
	
	

-- | Identify primitive operations.
corePrim
	:: (?args ::	[Arg])
	-> Tree			-- ^ core tree
	-> IO Tree
	
corePrim cTree
 = do	let cTree'	= primTree cTree
 	dumpCT DumpCorePrim "core-prim" cTree'
	return cTree'


-- | Local unboxing optimisation.
coreBoxing
	:: (?args :: [Arg])
	-> Set Var		-- ^ vars defined at top level
	-> Tree			-- ^ core tree
	-> Tree			-- ^ header tree
	-> IO Tree
	
coreBoxing topVars cSource cHeader
 = do	let cBoxing	= coreBoxingTree topVars cSource cHeader
 	dumpCT DumpCoreBoxing "core-boxing" cBoxing
	return	cBoxing 	


-- Full laziness optimisation.
coreFullLaziness
	:: (?args ::	[Arg])
	-> Module		-- ^ name of current module
	-> Tree			-- ^ core tree
	-> Tree			-- ^ header tree
	-> IO Tree		
	
coreFullLaziness
	moduleName
	cTree
	cHeader

 | elem OptFullLaziness ?args
 = do
	let appMap	= slurpAppGraph    cTree cHeader
	dumpDot GraphApps "apps"
		$ dotAppGraph appMap

	let cTree'	= fullLazinessTree moduleName cHeader cTree
	dumpCT DumpCoreFullLaziness "core-full-laziness" (eraseModuleTree cTree')
	-- eraseModuleTree

	return cTree'

 | otherwise
 =	return cTree



-- Function inlining.
coreInline
	:: (?args :: [Arg])
	-> Tree			-- ^ core tree
	-> Tree			-- ^ header tree
	-> [Var]		-- ^ bindings to inline
	-> IO Tree
	
coreInline
	cTree
	cHeader
	inlineVars

 | elem OptInline ?args
 = do	let cTree'	= inlineTree cTree cHeader inlineVars
 	
	dumpCT DumpCoreInline "core-inline" (eraseModuleTree cTree')
	
	return cTree'

 | otherwise
 =	return	cTree 
	


-- | Check the tree for syntactic problems that won't be caught by type checking.
coreLint
	:: (?args :: 	[Arg])
	-> String		-- ^ stage name
	-> Tree 		-- ^ core tree
	-> Tree 		-- ^ header tree
	-> IO ()
	
coreLint stage cTree cHeader
 | elem LintCore ?args
 = do	let errs	= lintTree (cHeader ++ cTree)

	case errs of 
	 []	-> return ()
	 errs	
	  ->	panic stage
	  		$ catInt "\n" errs
	
 | otherwise
 = 	return ()


	
-- | Lift nested functions to top level.
coreLambdaLift
	:: (?args :: [Arg])	
	-> Tree			-- ^ core tree
	-> Tree			-- ^ header tree

	-> IO	( Tree		-- the new source tree, old binds + new binds
		, Set Var)	-- the the vars of lifted bindings
	
	
coreLambdaLift cSource cHeader
 = do	
 	let (cBinds, cOther)
			= partition ((=@=) PBind{}) cSource
 
 	let vsBoundTop	= Set.fromList
			$ catMap slurpBoundVarsP (cSource ++ cHeader)
 
 	let (cBinds_lifted, cBinds_new)
		= lambdaLiftTree 
			cBinds
			Map.empty
			vsBoundTop
			
			
	let cLifted	= cOther ++ cBinds_lifted ++ cBinds_new
			
	let vsBinds_new	= catMap slurpBoundVarsP cBinds_new
			
	dumpCT DumpCoreLifted "core-lifted" 		cLifted
	dumpS  DumpCoreLifted "core-lifted-new-vars"	(show vsBinds_new)
	dumpS  DumpCoreLifted "core-lifted-vsBoundTop"	(catInt "\n" $ map show $ sort $ Set.toList vsBoundTop)
			
	return	( cLifted
		, Set.fromList vsBinds_new)



-- | Convert data structure labels to offsets.
coreLabelIndex
	:: (?args :: [Arg])
	-> Map Var CtorDef
	-> Tree			-- source tree
	-> IO Tree
	
coreLabelIndex mapCtorDefs cTree
 = do	let cIndex	= labelIndexTree mapCtorDefs cTree
 	
	dumpCT DumpCoreLabelIndex "core-labelIndex" cIndex
	return	cIndex



-- | Do dependency ordering of CAFs.
coreSequence
	:: (?args :: [Arg])
	-> Tree			-- ^ core tree
	-> Tree			-- ^ header tree
	-> IO Tree

coreSequence cSource cHeader
 = do	
 	-- sequence the tree
	let tree'	= sequenceCafsTree cSource
 	dumpCT DumpCoreSequence "core-sequence" tree'

	-- emit super deps
	let superDeps	= slurpSuperDepsTree cSource
	let superDepsG	= dotSuperDeps superDeps
	dumpDot GraphSuperDeps "super-deps" $ pretty superDepsG

	return tree'



-- | Identify partial applications and insert calls to explicitly create and apply thunks.
curryCall
	:: (?args :: [Arg])
	-> Tree			-- ^ core tree
	-> Tree			-- ^ header tree
	-> IO (Tree, Set Var)	-- ^ transformed tree, caf vars

curryCall cSource cHeader
 = do	let supers	= slurpSupersTree (cHeader ++ cSource)
  	let cafs	= Map.filter isCafP_opType supers
  	let cCurryCall	= curryTree cHeader cSource supers

	dumpCT DumpCoreCurry "core-curry" cCurryCall

	dumpS  DumpCoreCurry "core-curry-supers"
		("-- names of supers\n"
		++ (catInt "\n" $ sort $ map show $ map fst $ Map.toList supers))
	
	dumpS  DumpCoreCurry "core-curry-cafs"
		("-- names of supers which are also CAFs\n"
		++ (catInt "\n" $ sort $ map show $ map fst $ Map.toList cafs))
	
	let cafVars	= Set.fromList $ Map.keys cafs
	
	return	(cCurryCall, cafVars)


					
-- | Share constant constructors of airity zero.
coreAtomise
	:: (?args :: [Arg])
	-> Tree			-- ^ core tree
	-> Tree			-- ^ header tree
	-> IO Tree

coreAtomise cSource cHeader
 = do	let cAtomise	= atomiseTree cSource cHeader
 	dumpCT DumpCoreAtomise "core-atomise" cAtomise
	
	return	cAtomise



-- | Erase type information in preparation for conversion to Abstract-C.
coreDitch
	:: (?args :: [Arg])
	-> Tree			-- ^ core tree
	-> IO Tree		

coreDitch cSource
 = do	let cDitch	= crushTree $ ditchTree cSource
 	dumpCT DumpCoreDitch "core-ditch" cDitch
	
	return cDitch
	
	

-- | Convert Core-IR to Abstract-C
toSea
	:: (?args :: [Arg])
	-> Tree			-- ^ core tree
	-> Tree			-- ^ header tree

	-> IO 	( E.Tree ()	-- sea source tree
		, E.Tree ())	-- sea header tree

toSea	cTree cHeader

 = do
	let appMap	= slurpAppGraph    cTree cHeader
	dumpDot GraphApps "apps-final"
		$ dotAppGraph appMap

	let mapCtorDefs	= Map.union 
				(slurpCtorDefs cTree)
				(slurpCtorDefs cHeader)

	let eTree	= toSeaTree mapCtorDefs cTree
	dumpET DumpSea "sea-source"
		$ E.eraseAnnotsTree eTree

	let eHeader	= toSeaTree mapCtorDefs cHeader
	dumpET DumpSea "sea-header"
		$ E.eraseAnnotsTree eHeader

 	return (eTree, eHeader)
		

