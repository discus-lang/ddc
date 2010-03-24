{-# OPTIONS -fwarn-unused-imports #-}

-- | Wrappers for the compiler stages dealing with the Core IR
--	These wrappers are responsible for calling the functions that actually
--	implement the various transforms, and for dumping debugging info.
--
module Main.Core
	( coreNormalDo
	, coreSnip
	, coreDict
	, coreReconstruct
	, coreBind
	, coreThread
	, corePrim
	, coreSimplify
	, coreLint
	, coreLambdaLift
	, coreLabelIndex
	, curryCall
	, toSea)
where
import Core.Exp 	
import Core.Util
import Core.Glob
import Core.ToSea.Sequence
import Main.Dump
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Main.Arg
import DDC.Var
import Core.Block			(blockTree)
import Core.Crush			(crushTree)
import Core.Dictionary			(dictTree)
import Core.Reconstruct			(reconTree)
import Core.Bind			(bindTree)
import Core.Thread			(threadTree)
import Core.Prim			(primTree)
import Core.Lint			(lintTree)
import Core.Lift			(lambdaLiftTree)
import Core.LabelIndex			(labelIndexTree)
import Core.Curry			(curryTree)
import Core.ToSea			(toSeaTree)
import Data.Foldable			(foldr)
import Util				hiding (foldr)
import Prelude				hiding (foldr)
import qualified Core.Optimise.Simplify	as Simplify
import qualified Core.Float		as Float
import qualified Core.Snip		as Snip
import qualified Type.Util.Environment	as Env
import qualified Sea.Exp		as E
import qualified Sea.Util		as E
import qualified Data.Map		as Map
import qualified Data.Set		as Set
import qualified Data.Sequence		as Seq


-- | Convert to A-Normal form.
coreSnip
	:: (?args	:: [Arg])
	=> (?pathSourceBase :: FilePath)
	=> String 		-- ^ stage name
	-> String 		-- ^ unique
	-> Set Var 		-- ^ vars bound at top level
	-> Tree			-- ^ core tree
	-> IO Tree
	
coreSnip stage unique topVars tree
 = do	
	-- snip exprs out of fn arguments
	let snipTable	= Snip.Table
			{ Snip.tableTopVars		= topVars
			, Snip.tablePreserveTypes	= False }
			
	let treeSnip	= Snip.snipTree snipTable ("x" ++ unique) tree
	dumpCT DumpCoreSnip (stage ++ "-snip")  treeSnip
	
	return treeSnip


-- | Normalise use of do blocks in the code
coreNormalDo stage unique tree
 = do	
 	-- ensure all exprs are wrapped in do blocks.
 	let treeBlock	= blockTree tree
 	dumpCT DumpCoreBlock (stage ++ "-block") treeBlock

	-- crush nested do exprs
	let treeCrush	= crushTree treeBlock
 	dumpCT DumpCoreCrush (stage ++ "-crush") treeCrush

	return	treeCrush


-- | Resolve calls to overloaded functions.
coreDict
	:: (?args	:: [Arg])
	=> (?pathSourceBase :: FilePath)
	=> Tree 		-- ^ header tree
	-> Tree			-- ^ core tree
	-> IO Tree

coreDict hTree sTree
 = do	let tree'	= dictTree hTree sTree
 	dumpCT DumpCoreDict "core-dict" tree'
	return	tree'


-- | Reconstruct and check type information.
coreReconstruct
	:: (?args	:: [Arg])
	=> (?pathSourceBase :: FilePath)
	=> String		-- ^ stage name
	-> Tree			-- ^ header tree
	-> Tree			-- ^ core tree
	-> IO Tree
	
coreReconstruct name cHeader cTree
 = do	let table	= 
 		Env.emptyEnv
 		{ Env.envDropStmtEff	= False }
 
 	let cTree'	= {-# SCC "Core.Reconstruct" #-} 
 			   reconTree table cHeader cTree
 	dumpCT DumpCoreRecon name cTree'
	return	cTree'

	
-- | Bind local regions.
coreBind
	:: (?args ::	[Arg])
	=> (?pathSourceBase :: FilePath)
	=> ModuleId
	-> String		-- ^ unique
	-> (Map Var [Var])	-- ^ map of class constraints on each region
				--	eg (%r1, [Lazy, Const])
	-> Set Var		-- the regions with global lifetimes which should be bound 
				--	at top level.
	-> Tree	-> IO Tree
	
coreBind
	mod
	unique	
	classMap
	rsGlobal
	cSource
 = do
 	let tree' = {-# SCC "Core.Bind" #-}
	            bindTree mod unique classMap rsGlobal cSource
	
	dumpCT DumpCoreBind "core-bind" tree'

	dumpS  DumpCoreBind "core-bind--rsGlobal" 
		$ catInt "\n"
		$ map pprStrPlain $ Set.toList rsGlobal
	
	return tree'


-- | Thread through witness variables.
coreThread
	:: (?args :: [Arg])
	=> (?pathSourceBase :: FilePath)
	=> Tree 		-- ^ header tree
	-> Tree 		-- ^ core tree
	-> IO Tree
	
coreThread hTree cTree
 = do	let tree'	= {-# SCC "Core.Thread" #-} 
 			  threadTree hTree cTree
 
 	dumpCT DumpCoreThread "core-thread" tree'
	return tree'
	

-- | Identify primitive operations.
corePrim
	:: (?args ::	[Arg])
	=> (?pathSourceBase :: FilePath)
	=> Tree			-- ^ core tree
	-> IO Tree
	
corePrim cTree
 = do	let cTree'	= primTree cTree
 	dumpCT DumpCorePrim "core-prim" cTree'
	return cTree'


-- | Do core simplification
coreSimplify
	:: (?args :: [Arg])
	=> (?pathSourceBase :: FilePath)
	=> String		-- ^ unique
	-> Set Var		-- ^ vars defined at top level
	-> Tree			-- ^ core tree
	-> Tree			-- ^ header tree
	-> IO Tree
	
coreSimplify unique topVars cSource cHeader
 = do	when (elem Verbose ?args)
	 $ do	putStr $ "  * Optimise.Simplify\n"
 
 	let (cSimplify, statss)
 		= Simplify.coreSimplifyTree unique topVars cSource

	when (elem Verbose ?args)
	 $ do	putStr	$ pprStrPlain	$ "\n" %!% statss % "\n\n"

	-- when dumping our state, refloat let bindings so we can see 
	--	where the simplifier gave up.
	when (elem DumpCoreSimplify ?args)
	 $ do	let (_, cFloat)	= Float.floatBindsTreeUse cSimplify
	 	dumpCT DumpCoreSimplify "core-simplify"  cSimplify
		dumpCT DumpCoreSimplify "core-simplify--refloat" cFloat

		(case takeLast statss of
		   Just stats	-> dumpS DumpCoreSimplify "core-simplify--missedUnboxing" 
		   			(pprStrPlain $ "\n" %!% map ppr (reverse $ Float.statsMissedUnboxing 
									(Simplify.statsFloat stats)))
		   Nothing	-> return ())

		dumpS DumpCoreSimplify "core-simplify--stats"
			(pprStrPlain	$ "\n" %!% statss % "\n\n")

	return	cSimplify
	

-- | Check the tree for syntactic problems that won't be caught by type checking.
coreLint
	:: (?args :: 	[Arg])
	=> (?pathSourceBase :: FilePath)
	=> String		-- ^ stage name
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
	=> (?pathSourceBase :: FilePath)
	=> Tree			-- ^ core tree
	-> Tree			-- ^ header tree

	-> IO	( Tree		-- the new source tree, old binds + new binds
		, Set Var)	-- the the vars of lifted bindings
	
	
coreLambdaLift cSource cHeader
 = do	
	let isPBind pp
		= case pp of
			PBind{}	-> True
			_	-> False
			
 	let (cBinds, cOther)
			= partition isPBind cSource
 
 	let vsBoundTop	= Set.fromList
			$ catMap slurpBoundVarsP (cSource ++ cHeader)
 
 	let (cBinds_lifted, cBinds_new)
		= lambdaLiftTree 
			cBinds
			Map.empty
			vsBoundTop
			
			
	let cLifted	= cOther ++ cBinds_lifted ++ cBinds_new
			
	let vsBinds_new	= catMap slurpBoundVarsP cBinds_new
			
	dumpCT DumpCoreLift "core-lift" 		cLifted
	dumpS  DumpCoreLift "core-lift--new-vars"	(show vsBinds_new)
	dumpS  DumpCoreLift "core-lift--vsBoundTop"	(catInt "\n" $ map show $ sort $ Set.toList vsBoundTop)
			
	return	( cLifted
		, Set.fromList vsBinds_new)


-- | Convert data structure labels to offsets.
coreLabelIndex
	:: (?args :: [Arg])
	=> (?pathSourceBase :: FilePath)
	=> Map Var CtorDef
	-> Tree			-- source tree
	-> IO Tree
	
coreLabelIndex mapCtorDefs cTree
 = do	let cIndex	= labelIndexTree mapCtorDefs cTree
 	
	dumpCT DumpCoreLabelIndex "core-labelIndex" cIndex
	return	cIndex


-- | Identify partial applications and insert calls to explicitly create and apply thunks.
curryCall
	:: (?args :: [Arg])
	=> (?pathSourceBase :: FilePath)
	=> Tree				-- ^ source tree
	-> Tree				-- ^ header tree
	-> Glob				-- ^ source glob
	-> Glob				-- ^ header glob
	-> IO Tree			-- ^ transformed core tree

curryCall cSource cHeader cgSource cgHeader
 = do	let cCurryCall	= curryTree cHeader cSource cgHeader cgSource
	dumpCT DumpCoreCurry "core-curry" cCurryCall
	return	cCurryCall

			
-- | Convert Core-IR to Abstract-C
toSea	:: (?args	    :: [Arg])
	=> (?pathSourceBase :: FilePath)
	=> String		-- unique
	-> Glob			-- ^ header glob
	-> Glob			-- ^ source glob
	-> IO 	( E.Tree ()	-- sea source tree
		, E.Tree ())	-- sea header tree

toSea unique cgHeader cgSource
 = do	eCafOrder	<- slurpCafInitSequence cgSource

	case eCafOrder of
	 Left vsRecursive
	  -> exitWithUserError ?args
	 	$ ["Values may not be mutually recursive.\n"
		% "     offending variables: " % vsRecursive % "\n\n"]

	 Right vsCafOrdering
	  -> toSea_withCafOrdering unique cgHeader cgSource vsCafOrdering

toSea_withCafOrdering unique cgHeader cgSource vsCafOrdering
 = do
	-- Partition the bindings into CAFs and non-CAFs 
	let cgSource_binds = globBind cgSource
	let (  cgSource_binds_cafs
	     , cgSource_binds_nonCafs)	
			= Map.partition isCafP cgSource_binds
			
	-- Order the CAFs by the given CAF initilization order.
	let cgSource_binds_orderedCafs
		= foldl' (\psCafs v
			    -> let  Just pCaf	= Map.lookup v cgSource_binds_cafs
			       in   psCafs Seq.|> pCaf)
		   	Seq.empty
			vsCafOrdering
		
	-- All the bindings with ordered CAFs out the front.
	let cSource_binds_ordered
		=      cgSource_binds_orderedCafs 
		Seq.>< (Seq.fromList $ Map.elems cgSource_binds_nonCafs)
			
	let cSource_nobinds	
		= seqOfGlob (cgSource { globBind = Map.empty })

	let cSource'		
		=      cSource_binds_ordered 
		Seq.>< cSource_nobinds
			
	-- conversion
	let eTree	= toSeaTree (unique ++ "S") cSource'
	let eTree_list	= foldr (:) [] eTree
	dumpET DumpSea "sea--source" $ E.eraseAnnotsTree eTree_list

	let eHeader	 = toSeaTree (unique ++ "H") $ seqOfGlob cgHeader
	let eHeader_list = foldr (:) [] eHeader
	dumpET DumpSea "sea--header" $ E.eraseAnnotsTree eHeader_list

 	return (  eTree_list, eHeader_list)
		

