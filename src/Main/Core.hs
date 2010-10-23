{-# OPTIONS -fwarn-unused-imports #-}

-- | Wrappers for the compiler stages dealing with the Core IR.
--	These wrappers are responsible for calling the functions that actually
--	implement the transforms, and for dumping debugging info.
--
module Main.Core
	( coreNormaliseDo
	, coreSnip
	, coreBind
	, coreThread
	, coreDictionary
	, corePrim
	, coreSimplify
	, coreLint
	, coreLambdaLift
	, coreLabelIndex
	, coreCurryCall
	, toSea)
where
import Core.Util
import Core.ToSea.Sequence
import Main.Dump
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Main.Arg
import DDC.Core.Glob
import DDC.Core.Check			(checkGlobs)
import DDC.Var
import Core.Block			(blockGlob)
import Core.Crush			(crushGlob)
import Core.Dictionary			(dictGlob)
import Core.Bind			(bindGlob)
import Core.Thread			(threadGlob)
import Core.Prim			(primGlob)
import Core.Simplify			(simplifyGlob)
import Core.Lift			(lambdaLiftGlob)
import Core.LabelIndex			(labelIndexGlob)
import Core.Curry			(curryGlob)
import Core.ToSea			(toSeaTree)
import Data.Foldable			(foldr)
import Util				hiding (foldr)
import Prelude				hiding (foldr)
import qualified Core.Snip		as Snip
import qualified DDC.Sea.Exp		as E
import qualified Sea.Util		as E
import qualified Data.Map		as Map
import qualified Data.Set		as Set
import qualified Data.Sequence		as Seq


-- | Normalise the form of expressions in a glob to use do blocks.
--   This makes the right of every pattern alternative, and the body of every function do.
--   This is the form that coreBind below needs.
coreNormaliseDo
	:: (?args :: [Arg])
	=> (?pathSourceBase :: FilePath)
	=> String		-- ^ Stage Name.
	-> String		-- ^ Unique.
	-> Glob			-- ^ Module Glog.
	-> IO Glob
	
coreNormaliseDo stage unique cgModule
 = do	
 	-- ensure all exprs are wrapped in do blocks.
 	let cgModule_blocked	= blockGlob cgModule
 	dumpCT DumpCoreBlock (stage ++ "-block") 
		$ treeOfGlob cgModule_blocked

	-- crush nested do exprs
	let cgModule_crushed	= crushGlob cgModule_blocked
 	dumpCT DumpCoreCrush (stage ++ "-crush") 
		$ treeOfGlob cgModule_crushed

	return	cgModule_crushed


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
	-> Glob
	-> IO Glob
	
coreBind mod unique classMap rsGlobal cgModule
 = do
 	let cgModule' 
		= {-# SCC "Core.Bind" #-}
	          bindGlob mod unique classMap rsGlobal cgModule
	
	dumpCT DumpCoreBind "core-bind"
		$ treeOfGlob cgModule'

	dumpS  DumpCoreBind "core-bind--rsGlobal" 
		$ catInt "\n"
		$ map pprStrPlain 
		$ Set.toList rsGlobal
	
	return cgModule'


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



-- | Resolve calls to overloaded functions.
coreDictionary
	:: (?args	:: [Arg])
	=> (?pathSourceBase :: FilePath)
	=> Glob 		-- ^ Header glob.
	-> Glob			-- ^ Module glob.
	-> IO Glob

coreDictionary cgHeader cgModule
 = do	let cgModule'	= dictGlob cgHeader cgModule

 	dumpCT DumpCoreDict "core-dict"
		$ treeOfGlob cgModule'

	return	cgModule'
	
	
-- | Thread through witness variables.
coreThread
	:: (?args :: [Arg])
	=> (?pathSourceBase :: FilePath)
	=> Glob 		-- ^ Header Glob.
	-> Glob 		-- ^ Module Glob.
	-> IO Glob
	
coreThread cgHeader cgModule
 = do	let cgModule'	= {-# SCC "Core.Thread" #-} 
 			  threadGlob cgHeader cgModule
 
 	dumpCT DumpCoreThread "core-thread" 
		$ treeOfGlob cgModule'

	return cgModule'
	

-- | Identify primitive operations.
corePrim
	:: (?args ::	[Arg])
	=> (?pathSourceBase :: FilePath)
	=> Glob			-- ^ Header glob.
	-> Glob			-- ^ Module glob.
	-> IO Glob
	
corePrim cgHeader cgModule
 = do	let cgModule'	= primGlob cgModule

 	dumpCT DumpCorePrim "core-prim" 
		$ treeOfGlob cgModule'

	return cgModule'


-- | Do core simplification
coreSimplify
	:: (?args :: [Arg])
	=> (?pathSourceBase :: FilePath)
	=> String		-- ^ unique
	-> Glob			-- ^ Header glob.
	-> Glob			-- ^ Module glob.
	-> IO Glob
	
coreSimplify unique cgHeader cgModule
 = do	let (cgModule', statss)
 		= simplifyGlob unique cgHeader cgModule

	when (elem Verbose ?args)
	 $ do	putStr	$ pprStrPlain	$ "\n" %!% statss % "\n\n"

	-- when dumping our state, refloat let bindings so we can see 
	--	where the simplifier gave up.
	when (elem DumpCoreSimplify ?args)
	 $ do	dumpCT DumpCoreSimplify "core-simplify" $ treeOfGlob cgModule'

{-		dumpCT DumpCoreSimplify "core-simplify--refloat" cFloat
		let (_, cFloat)	= Float.floatBindsTreeUse cSimplify
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
	-> Glob 		-- ^ core tree
	-> Glob 		-- ^ header tree
	-> IO Glob
	
coreLint stage cgHeader cgModule
 = do	let cgModule'	
		= checkGlobs ("Compile.coreLint." ++ stage) cgHeader cgModule 

	dumpCT DumpCoreLint stage 		
		$ treeOfGlob cgModule'		

	return cgModule'


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
		= lambdaLiftGlob 
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
 = do	let cgModule'	= labelIndexGlob cgHeader cgModule
 	
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
	let cgModule'	= curryGlob optTailCall cgHeader cgModule

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

	-- For the toSea transform we need to know which bindings are CAFs
	let vsCafs	= Set.union 
				(Set.fromList $ Map.keys cgModule_binds_cafs)
				(Set.fromList $ Map.keys $ Map.filter isCafP $ globExtern cgHeader)
	
	dumpS DumpSea "sea--vsCafs"  $ pprStrPlain $ ppr vsCafs
	
	-- conversion
	let eTree	= toSeaTree (unique ++ "S") vsCafs cModule'
	let eTree_list	= foldr (:) [] eTree
	dumpET DumpSea "sea--source" $ E.eraseAnnotsTree eTree_list

	let eHeader	 = toSeaTree (unique ++ "H") vsCafs $ seqOfGlob cgHeader
	let eHeader_list = foldr (:) [] eHeader
	dumpET DumpSea "sea--header" $ E.eraseAnnotsTree eHeader_list

 	return (  eTree_list, eHeader_list)
		

