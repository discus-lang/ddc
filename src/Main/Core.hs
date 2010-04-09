{-# OPTIONS -fwarn-unused-imports #-}

-- | Wrappers for the compiler stages dealing with the Core IR.
--	These wrappers are responsible for calling the functions that actually
--	implement the transforms, and for dumping debugging info.
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
	, coreCurryCall
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
import Core.Reconstruct			(reconTreeWithEnv)
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
--import qualified Core.Float		as Float
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
	-> Glob			-- ^ Header glob.
	-> Glob			-- ^ Module glob.
	-> IO Glob
	
coreSnip stage unique cgHeader cgModule
 = do	
	-- snip exprs out of fn arguments
	let snipTable	= Snip.Table
			{ Snip.tableHeaderGlob		= cgHeader
			, Snip.tableModuleGlob		= cgModule
			, Snip.tablePreserveTypes	= False }
			
	let cgModule'	= Snip.snipGlob snipTable ("x" ++ unique) cgModule

	dumpCT DumpCoreSnip (stage ++ "-snip")  
		$ treeOfGlob cgModule'
	
	return cgModule'


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
	=> String		-- ^ Stage name.
	-> Glob			-- ^ Header module.
	-> Glob			-- ^ Core   module.
	-> IO Glob
	
coreReconstruct name cgHeader cgModule
 = do	let table	
		= Env.emptyEnv
 		{ Env.envDropStmtEff	= False }
 
 	let cgModule'	= {-# SCC "Core.Reconstruct" #-} 
 			   reconTreeWithEnv table cgHeader cgModule
 	dumpCT DumpCoreRecon name 
		$ treeOfGlob cgModule'

	return	cgModule'

	
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
	=> Glob 		-- ^ Header Glob.
	-> Glob 		-- ^ Module Glob.
	-> IO Glob
	
coreThread cgHeader cgModule
 = do	let cgModule'	= {-# SCC "Core.Thread" #-} 
 			  threadTree cgHeader cgModule
 
 	dumpCT DumpCoreThread "core-thread" 
		$ treeOfGlob cgModule'

	return cgModule'
	

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
	-> Glob			-- ^ Header glob.
	-> Glob			-- ^ Source glob.
	-> IO Glob
	
coreSimplify unique cgHeader cgModule
 = do	let (cgModule', statss)
 		= Simplify.coreSimplifyGlob unique cgHeader cgModule

	when (elem Verbose ?args)
	 $ do	putStr	$ pprStrPlain	$ "\n" %!% statss % "\n\n"

	-- when dumping our state, refloat let bindings so we can see 
	--	where the simplifier gave up.
{-	when (elem DumpCoreSimplify ?args)
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
-}
	return	cgModule'
	

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
	=> Glob				-- ^ Header glob.
	-> Glob				-- ^ Module glob.
	-> IO	( Glob			-- transformed module glob, including new lifted bindings.
		, Set Var)		-- the vars of the new bindings.
	
coreLambdaLift cgHeader cgModule
 = do	
 	let (cgModule', vsNewLambdaLifted)
		= lambdaLiftTree 
			cgHeader
			cgModule
						
	dumpCT DumpCoreLift "core-lift" 		
		$ treeOfGlob cgModule'

	dumpS  DumpCoreLift "core-lift--new-bindings"	
		$ show vsNewLambdaLifted
			
	return	( cgModule'
		, vsNewLambdaLifted)


-- | Convert data structure labels to offsets.
coreLabelIndex
	:: (?args :: [Arg])
	=> (?pathSourceBase :: FilePath)
	=> Glob				-- ^ Header glob.
	-> Glob				-- ^ Module glob.
	-> IO Glob
	
coreLabelIndex cgHeader cgModule
 = do	let cgModule'	= labelIndexTree cgHeader cgModule
 	
	dumpCT DumpCoreLabelIndex "core-labelIndex" 
		$ treeOfGlob cgModule'

	return	cgModule'


-- | Identify partial applications and insert calls to explicitly create and apply thunks.
coreCurryCall
	:: (?args :: [Arg])
	=> (?pathSourceBase :: FilePath)
	=> Glob				-- ^ Header glob.
	-> Glob				-- ^ Module glob.
	-> IO Glob

coreCurryCall cgHeader cgModule
 = do	let optTailCall	= elem OptTailCall ?args
	let cgModule'	= curryTree optTailCall cgHeader cgModule

	dumpCT DumpCoreCurry "core-curry" 
		$ treeOfGlob cgModule'

	return	cgModule'

			
-- | Convert Core-IR to Abstract-C
toSea	:: (?args	    :: [Arg])
	=> (?pathSourceBase :: FilePath)
	=> String			-- ^ Unique.
	-> Glob				-- ^ Header glob.
	-> Glob				-- ^ Module glob.
	-> IO 	( E.Tree ()		-- sea source tree
		, E.Tree ())		-- sea header tree

toSea unique cgHeader cgModule
 = do	eCafOrder	<- slurpCafInitSequence cgModule

	case eCafOrder of
	 Left vsRecursive
	  -> exitWithUserError ?args
	 	$ ["Values may not be mutually recursive.\n"
		% "     offending variables: " % vsRecursive % "\n\n"]

	 Right vsCafOrdering
	  -> toSea_withCafOrdering unique cgHeader cgModule vsCafOrdering

toSea_withCafOrdering unique cgHeader cgModule vsCafOrdering
 = do
	-- Partition the bindings into CAFs and non-CAFs 
	let cgModule_binds = globBind cgModule
	let (  cgModule_binds_cafs
	     , cgModule_binds_nonCafs)	
			= Map.partition isCafP cgModule_binds
			
	-- Order the CAFs by the given CAF initilization order.
	let cgModule_binds_orderedCafs
		= foldl' (\psCafs v
			    -> let  Just pCaf	= Map.lookup v cgModule_binds_cafs
			       in   psCafs Seq.|> pCaf)
		   	Seq.empty
			vsCafOrdering
		
	-- All the bindings with ordered CAFs out the front.
	let cModule_binds_ordered
		=      cgModule_binds_orderedCafs 
		Seq.>< (Seq.fromList $ Map.elems cgModule_binds_nonCafs)
			
	let cModule_nobinds	
		= seqOfGlob (cgModule { globBind = Map.empty })

	let cModule'		
		=      cModule_binds_ordered 
		Seq.>< cModule_nobinds
			
	-- conversion
	let eTree	= toSeaTree (unique ++ "S") cModule'
	let eTree_list	= foldr (:) [] eTree
	dumpET DumpSea "sea--source" $ E.eraseAnnotsTree eTree_list

	let eHeader	 = toSeaTree (unique ++ "H") $ seqOfGlob cgHeader
	let eHeader_list = foldr (:) [] eHeader
	dumpET DumpSea "sea--header" $ E.eraseAnnotsTree eHeader_list

 	return (  eTree_list, eHeader_list)
		

