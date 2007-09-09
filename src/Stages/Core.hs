
module Stages.Core
	( coreBlock
	, coreSnip
	, coreCrush
	, coreDict
	, coreReconstruct
	, coreBind
--	, coreMaskEffs
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

-----
import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

-----
import Util
import qualified Shared.Var	as Var
import Shared.Var		(Var, Module)
import Shared.Error
import qualified Shared.Unique	as Unique

-----
import Core.Exp 	
import Core.Util
import Core.Util.Slurp

import Core.Block			(blockTree)
import Core.Snip			(snipTree)
import Core.Crush			(crushTree)
import Core.Dictionary			(dictTree)
import Core.Reconstruct			(reconstructTree)
import Core.Bind			(bindTree)
import Core.Prim			(primTree)
import Core.Lint			(lintTree)
import Core.FreezeStatic		(gatherStaticRegions)
import Core.Lift			(lambdaLiftTree)
import Core.LabelIndex			(labelIndexTree)
import Core.Curry			(curryTree, slurpSupersTree, isCafP_opType)
import Core.Ditch			(ditchTree)
import Core.ToSea			(toSeaTree)

-- import Core.Optimise.MaskEffs		(maskEffsTree)
import Core.Optimise.Boxing		(coreBoxingTree)
import Core.Optimise.Atomise		(atomiseTree)
import Core.Optimise.FullLaziness	(fullLazinessTree)
import Core.Optimise.Inline		(inlineTree)

import Core.Graph
import Core.Plate.Util			(eraseModuleTree)
import Core.Sequence			(slurpSuperDepsTree, dotSuperDeps, sequenceCafsTree)

import qualified Sea.Exp	as E
import qualified Sea.Util	as E
import Sea.Pretty

import Dot.Pretty
import Dot.Graph

import Main.Arg
import Main.Path

import Stages.Dump

-----
coreBlock
	:: (?args	:: [Arg])
	-> Tree	-> IO Tree
	
coreBlock tree
 = do	let tree'	= blockTree tree
 	dumpCT DumpCoreBlock "core-block" tree'
	return tree'


-----
coreSnip 
	:: (?args	:: [Arg])
	-> String -> String -> Set Var -> Tree -> IO Tree

coreSnip stage unique topVars cSource
 = do	let tree'	= snipTree topVars ("x" ++ unique) cSource
	dumpCT DumpCoreSnip stage tree'
	return	tree'


-----
coreCrush
	:: (?args	:: [Arg])
	-> Tree -> IO Tree

coreCrush tree
 = do	let tree'	= crushTree tree
 	dumpCT DumpCoreCrush "core-crush" tree'
	return	tree'

-----
coreDict
	:: (?args	:: [Arg])
	-> Tree 
	-> Tree
	-> IO Tree

coreDict hTree sTree
 = do	let tree'	= dictTree hTree sTree
 	dumpCT DumpCoreDict "core-dict" tree'
	return	tree'


-----
coreReconstruct
	:: (?args	:: [Arg])
	-> String
	-> Tree				-- header tree
	-> Tree				-- core tree
	-> IO Tree
	
coreReconstruct name cHeader cTree
 = do	let cTree'	= reconstructTree cHeader cTree
 	dumpCT DumpCoreReconstruct name cTree'
	return	cTree'
	
----
coreBind
	:: (?args ::	[Arg])
	-> (Var -> Maybe [Class])	-- getFetters
	-> Tree	-> IO Tree
	
coreBind
	getFetters
	cSource
 = do
 	let tree'	
		= bindTree
			getFetters
			cSource
	
	dumpCT DumpCoreBind "core-bind" tree'

	return tree'

-----
{-
coreMaskEffs
	:: (?args ::	[Arg])
	-> Tree
	-> IO Tree
	
coreMaskEffs
	cTree
 = do
	let cTree'	= maskEffsTree cTree
	
	dumpCT DumpCoreMaskEffs "core-mask-effs" cTree'

 	return	cTree'
-}

-----
corePrim
	:: (?args ::	[Arg])
	-> Tree
	-> IO Tree
	
corePrim cTree
 = do	let cTree'	= primTree cTree
 	dumpCT DumpCorePrim "core-prim" cTree'
	return cTree'

-----
coreBoxing
	:: (?args :: [Arg])
	-> Set Var			-- vars defined at top level
	-> Tree				-- source tree
	-> Tree				-- header tree
	-> IO Tree
	
coreBoxing topVars cSource cHeader
 = do	let cBoxing	= coreBoxingTree topVars cSource cHeader
 	dumpCT DumpCoreBoxing "core-boxing" cBoxing
	return	cBoxing 	


-----
coreFullLaziness
	:: (?args ::	[Arg])
	-> Module			-- name of current module
	-> Tree				-- source tree
	-> Tree				-- header tree
	-> IO Tree			-- core tree after full laziness transform
	
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


-----
coreInline
	:: (?args :: [Arg])
	-> Tree
	-> Tree
	-> [Var]
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
	

-----
coreLint
	:: (?args :: 	[Arg])
	-> Tree -> Tree -> IO ()
	
coreLint cTree cHeader
 | elem LintCore ?args
 = do	let errs	= lintTree (cHeader ++ cTree)

	case errs of 
	 []	-> return ()
	 errs	
	  ->	panic "core-lint"
	  		$ catInt "\n" errs
			
	
 | otherwise
 = 	return ()

	
-----
coreLambdaLift
	:: (?args :: [Arg])	
	-> Tree			-- source tree
	-> Tree			-- header tree

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

-----
coreLabelIndex
	:: (?args :: [Arg])
	-> Map Var CtorDef
	-> Tree			-- source tree
	-> IO Tree
	
coreLabelIndex mapCtorDefs cTree
 = do	let cIndex	= labelIndexTree mapCtorDefs cTree
 	
	dumpCT DumpCoreLabelIndex "core-labelIndex" cIndex
	return	cIndex


-----
coreSequence
	:: (?args :: [Arg])
	-> Tree			-- source tree
	-> Tree			-- header tree
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


-----
curryCall
	:: (?args :: [Arg])
	-> Tree			-- source tree
	-> Tree			-- header tree
	-> IO (Tree, Set Var)	-- transformed tree, caf vars

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

					
-----
coreAtomise
	:: (?args :: [Arg])
	-> Tree			-- source tree
	-> Tree			-- header tree
	-> IO Tree

coreAtomise cSource cHeader
 = do	let cAtomise	= atomiseTree cSource cHeader
 	dumpCT DumpCoreAtomise "core-atomise" cAtomise
	
	return	cAtomise

-----
coreDitch
	:: (?args :: [Arg])
	-> Tree			-- source tree
	-> IO Tree		

coreDitch cSource
 = do	let cDitch	= crushTree $ ditchTree cSource
 	dumpCT DumpCoreDitch "core-ditch" cDitch
	
	return cDitch
	


-----
toSea
	:: (?args :: [Arg])
	-> Tree			-- core source tree
	-> Tree			-- core header tree

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
		

