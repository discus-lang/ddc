-----
-- Stages.Source
--
--	BUGS: sourceSlurpInline vars should check that vars to inline are in scope at top level
--
module Main.Source
	( Main.Source.parse
	, sourceSlurpFixTable
	, sourceSlurpInlineVars
	, defix
	, rename
	, sourceKinds
	, desugar
	, desugarProject
	, alias
	, slurpC
	, solveSquid
	, toCore )

where



-----
import qualified Shared.Var		as Var
import Shared.Var			(Var, Module)
import Shared.Base
import Shared.Error
import Shared.Pretty

-----
import qualified Type.Exp			as T
import qualified Type.Pretty			as T
import qualified Type.Solve			as Squid
import qualified Type.State			as Squid
import qualified Type.Export			as Squid
import qualified Type.Dump			as Squid

import qualified Constraint.Simplify		as N
import qualified Constraint.Exp			as N

import qualified Source.Token			as Token
import qualified Source.Rename			as S
import qualified Source.RenameM			as S
import qualified Source.RenameM			as Rename
import Source.Lexer				(scan)
import Source.Parser				(parse)
import Source.Slurp				(slurpFixTable, slurpKinds)
import Source.Defix				(defixP)
import Source.Desugar				(rewriteTree)
import Source.Alias				(aliasTree)
import Source.Exp
import Source.Token

import qualified Desugar.Exp			as D
import qualified Desugar.Slurp.State		as D
import qualified Desugar.Plate.Trans		as D
import Desugar.Slurp.Slurp			(slurpTreeM)
import Desugar.Project				(projectTree, ProjTable, slurpProjTable)
import Desugar.ToCore				(toCoreTree)

import qualified Core.Exp			as C
import qualified Core.Pretty			as C

import Main.Arg

import Main.Dump

-----
import qualified Data.Map		as Map
import Data.Map (Map)

import qualified Data.Set		as Set
import Data.Set	(Set)

import System.Exit
import System.IO

import Util


-----
-- stage = "Stages.Source"

-- | Parse source code.
parse 	:: (?args :: [Arg])
	-> (?pathSourceBase :: FilePath)
	-> FilePath			-- path of source file
	-> String			-- source of root module
	-> IO (Tree SourcePos)		-- source parse tree
			
parse	fileName
	source
 = do
	let tokens	= map (tokenSetFileName fileName)
			$ scan source
			
	let sParsed	= Source.Parser.parse tokens
	
	-- dump
	dumpST 	DumpSourceParse "source-parse" sParsed

	return	sParsed

tokenSetFileName name token
 = case token of
 	TokenP{}	-> token { Token.tokenFile = name }
	_		-> token

-- | Slurp out fixity table
sourceSlurpFixTable
	:: Tree SourcePos		-- source and header parse tree
	-> IO [FixDef SourcePos]	-- fixity table
		
sourceSlurpFixTable
	sTree
 = do
	let fixTable	= slurpFixTable sTree
	return	fixTable


-- | Slurp out table of bindings to inline
sourceSlurpInlineVars
	:: Tree SourcePos
	-> IO [Var]
	
sourceSlurpInlineVars 
	sTree
 = 
 	return	$ catMap
			(\p -> case p of
 				PPragma _ [XVar sp v, XList _ xs]
				 | Var.name v == "Inline"
				 , vs		 <- map (\(XVar sp v) -> v) xs
				 -> vs
			 
				_ -> [])
			sTree


-- | Write uses of infix operatiors to preix form.
defix	:: (?args :: [Arg])
	-> (?pathSourceBase :: FilePath)
	-> Tree	SourcePos		-- source parse tree
	-> [FixDef SourcePos]		-- fixity table
	-> IO (Tree SourcePos)		-- defixed parse tree, will have no more XInfix nodes.
	
defix	sParsed
	fixTable
 = do
 	let (sDefixed, errss)
			= unzip
			$ map (defixP fixTable)
			$ sParsed
	
	exitWithUserError ?args $ concat errss
	
	-- dump
	dumpST	DumpSourceDefix "source-defix" sDefixed
	
	return	 sDefixed
	

-- | Check scoping of variables and 
-- NOTE:
-- 	We need to rename infix defs _after_ foreign imports
--	We do this so that the Sea name for functions like (+ / primInt32Add)
--	which is present on foreign decls gets propagated to uses of these functions.
--
rename	:: (?args :: [Arg])
	-> (?pathSourceBase :: FilePath)
	-> 	[(Module, Tree SourcePos)]
	-> IO 	[(Module, Tree SourcePos)]


rename	mTrees
 = do
	let (mTrees', state')
		= runState (S.renameTrees mTrees)
		$ S.initRenameS

	-- dump
	let Just sTree	= liftM snd $ takeHead mTrees'

	dumpST 	DumpSourceRename "source-rename" sTree
	dumpST 	DumpSourceRename "source-rename--header" (concat $ map snd $ tail mTrees')

	-- exit after dumping, so we can see what's going on.
	exitWithUserError ?args $ S.stateErrors state'
	
	return mTrees'


-- Slurp out the kinds for user defined classes.
sourceKinds
	:: (?args :: [Arg])
	-> (?pathSourceBase :: FilePath)
	-> Tree SourcePos
	-> IO [(Var, Kind)]
	
sourceKinds sTree
 = do	let kinds	= slurpKinds sTree
--	dumpS DumpSourceKinds "source-kinds" 
--		(catMap (\(v, k) -> pprStr $ v %>> " :: " % k % ";\n") kinds)
		
	return	kinds


-----------------------
-- alias
--
alias 	:: (?args :: [Arg])
	-> (?pathSourceBase :: FilePath)
	-> Tree SourcePos
	-> IO (Tree SourcePos)
	
alias sTree
 = do
 	let sTree'	= aliasTree sTree
--	dumpST	DumpSourceAlias "source-alias" sTree'

	return	sTree'
	
	
-- | Convert from Source to Desugared IR.
desugar
	:: (?args :: [Arg])
	-> (?pathSourceBase :: FilePath)
	-> String			-- unique
	-> [(Var, Kind)]		-- kind table
	-> Tree	SourcePos		-- header tree
	-> Tree	SourcePos		-- source tree
	-> IO 	( D.Tree SourcePos
		, D.Tree SourcePos)
	
desugar unique kinds hTree sTree
 = do
	let kindMap	= Map.fromList kinds
	let (hTree', sTree', errors)	
			= rewriteTree unique kindMap hTree sTree
			
	-- dump
	dumpST DumpDesugar "desugar--header" 
		(map (D.transformN $ \a -> (Nothing :: Maybe ())) hTree')

	dumpST DumpDesugar "desugar--source" 
		(map (D.transformN $ \a -> (Nothing :: Maybe ())) sTree')

	when (not $ null errors)
	 $ exitWithUserError ?args errors
		
	return	(hTree', sTree')


-----------------------
-- desugarProject
-- 
desugarProject 
	:: (?args :: [Arg])
	-> (?pathSourceBase :: FilePath)
	-> String
	-> Module
	-> D.Tree SourcePos
	-> D.Tree SourcePos
	-> IO	( D.Tree SourcePos
		, ProjTable )

desugarProject unique moduleName headerTree sourceTree
 = do
	-- Snip down projection dictionaries and add default projections.
 	let (sourceTree', errors)
		= projectTree unique moduleName headerTree sourceTree 
	
	dumpST DumpDesugarProject "desugar-project"
		(map (D.transformN $ \a -> (Nothing :: Maybe ())) sourceTree')

	-- Slurp out the ProjTable
	let projTable	= slurpProjTable (headerTree ++ sourceTree')

	dumpS  DumpDesugarProject "desugar-project--dicts"
		(pprStrPlain $ "\n" %!% (map ppr $ Map.toList projTable))

	when (not $ null errors)
	 $ exitWithUserError ?args errors
		
	return (sourceTree', projTable)

	
	
-----------------------
-- slurpC
--
slurpC 	:: (?args :: [Arg])
	-> (?pathSourceBase :: FilePath)
	-> (D.Tree SourcePos)				-- source tree
	-> (D.Tree SourcePos)				-- header tree
	-> IO	( (D.Tree (Maybe (Type, Effect)))	-- source tree with type and effect annotations
		, [N.CTree]				-- type constraints
		, Map Var Var 				-- sigma table (map of value -> type vars)
		, Set Var				-- all the value vars used in the program that we'll want types for
		, Set Var)				-- type vars for all top level bindings in the source module
		
slurpC	sTree
	hTree

 = {-# SCC "slurpC" #-}
   do
	let state	= D.initCSlurpS 
	
	-- slurp constraints from the header
	let ((_, hctrs, _), state2)
			= runState (slurpTreeM hTree)
			$ state
		
	-- slurp constraints from the source 
	let ((source', sctrs, vsBound_source), state3)
			= runState (slurpTreeM sTree)
			$ state2

	-- handle errors arrising from constraint slurping
	exitWithUserError ?args $ D.stateErrors state3

	-- these are the vars we'll need types for during the Core->Desugar transform
	let vsTypesPlease = D.stateTypesRequest state3

	-- this is the table mapping value vars to type vars
	let sigmaTable	= D.stateVarType state3

	-- simplify source constraints (header constraints are already pretty simple)
	let sctrs_simple = N.simplify vsTypesPlease sctrs


	-- dump
	dumpST	DumpDesugarSlurp "desugar-slurp" source'
	
	dumpS	DumpTypeConstraints "type-constraints--source"
		$ (catInt "\n" $ map pprStrPlain sctrs)

	dumpS	DumpTypeConstraints "type-constraints--source-simple"
		$ (catInt "\n" $ map pprStrPlain sctrs_simple)
		
	dumpS	DumpTypeConstraints "type-constraints--header"
		$ (catInt "\n" $ map pprStrPlain hctrs)

	dumpS	DumpTypeConstraints "type-constraints--typesPlease"
		$ (catInt "\n" $ map pprStrPlain $ Set.toList vsTypesPlease)
		
	-- all the constraints we're passing to the inferencer
	let constraints	= hctrs ++ sctrs_simple

	--
	return	( source'
		, constraints
		, sigmaTable 
		, vsTypesPlease
		, vsBound_source)
	
	
-----
solveSquid :: (?args :: [Arg])
	-> (?pathSourceBase :: FilePath)
	-> [N.CTree]				-- type constraints
	-> Set Var				-- the TEC vars to infer TECs for	
	-> Map Var Var				-- sigma table
	
	-> IO 	( Map Var T.Type			-- inferred types
		, Map Var (InstanceInfo T.Type T.Type)	-- how each var was instantiated
		, Map Var (T.Kind, Maybe T.Type)	-- the vars that were quantified during type inference
							--	(with optional :> bound)
		, Set Var				-- the TREC vars which are free in the returned types
		, Map Var [Var]				-- map of constraints on each region
		, Map Var Var)				-- how projections were resolved
	
solveSquid 
	constraints
	vsTypesPlease
	sigmaTable

 = {-# SCC "solveSquid" #-}
   do
	-- The solver state gets dumped in real-time so we can see
	--	what's gone wrong if it crashes mid-stream.

	hTrace	<- dumpOpen DumpTypeSolve "type-solve--trace"
		
 	state	<- {-# SCC "solveSquid/solve" #-} Squid.squidSolve 
			?args
			constraints
			sigmaTable
			hTrace

	-- dump out the type graph
	--	do this before bailing on errors so we can see what's gone wrong.
	when (elem Verbose ?args)
	 $ putStr "  * Type: Dumping graph\n"
	 
	dGraph	<- evalStateT Squid.dumpGraph state
	dumpS	DumpTypeSolve  "type-solve--graph" dGraph

	-- stop the compiler and print out errors
	--	if there were any during type inference.
	case Squid.stateErrors state of

	 -- no errors, carry on
	 [] ->	solveSquid2 vsTypesPlease hTrace state

	 -- time to die
	 _ -> do
		-- flush the trace output to make sure it's written to the file.
		(case hTrace of
		 Just handle	-> hFlush handle
		 Nothing	-> return ())

		exitWithUserError ?args $ Squid.stateErrors state
		
		panic "Core.SolveSquid" "done already"

	 
solveSquid2 vsTypesPlease hTrace state
 = do	
	when (elem Verbose ?args)
	 $ putStr "  * Type: Exporting\n"

	-- extract out the stuff we'll need for conversion to core.
	(junk, state2)	<- {-# SCC "solveSquid/export" #-} runStateT 
				(Squid.squidExport vsTypesPlease) state

	let (typeTable, typeInst, quantVars, vsRegionClasses)
			= junk

	-- flush the trace output to make sure it's written to the file.
	(case hTrace of
	 Just handle	-> hFlush handle
	 Nothing	-> return ())

	-- report some state
	when (elem Verbose ?args)
	 $ do	putStr 	$ pprStrPlain
	 		$ "    - graph size: " 
	 		% Squid.graphClassIdGen (Squid.stateGraph state2) % "\n"

	when (elem Verbose ?args)
	 $ putStr "  * Type: Dumping final state\n"

	-- dump final solver state
	dumpS	DumpTypeSolve  "type-solve--types"
		$ catInt "\n\n"
		$ map pprStrPlain
		$ map (\(v, t) -> v % " ::\n" %> T.prettyTS t)
		$ Map.toList typeTable

	dumpS 	DumpTypeSolve   "type-solve--inst" 
		$ catInt "\n\n"
		$ map pprStrPlain
		$ map (\(v, inst) -> v % "\n" % inst % "\n")
		$ Map.toList typeInst

	dumpS	DumpTypeSolve	"type-solve--quantVars"
		$ catInt "\n"
		$ map pprStrPlain
		$ Map.toList quantVars

	dumpS	DumpTypeSolve	"type-solve--regionClasses"
		$ catInt "\n"
		$ map pprStrPlain
		$ Map.toList vsRegionClasses

	dumpS	DumpTypeSolve	"type-solve--varSub"
		$ catInt "\n"
		$ map pprStrPlain
		$ Map.toList (Squid.stateVarSub state2)

	let vsFree	= Set.empty

	-- the export process can find more errors
	when (not $ null $ Squid.stateErrors state2)
	 $ exitWithUserError ?args $ Squid.stateErrors state2

	-----
	return 	( typeTable
		, typeInst
		, quantVars
		, vsFree
		, vsRegionClasses
		, Squid.stateProjectResolve state2)



-----------------------
-- toCore
--
toCore 	:: (?args :: [Arg])
	-> (?pathSourceBase :: FilePath)
	-> D.Tree (Maybe (Type, Effect))		-- sourceTree
	-> D.Tree (Maybe (Type, Effect))		-- headerTree
	-> Map Var Var					-- sigmaTable
	-> Map Var T.Type				-- typeTable
	-> Map Var (T.InstanceInfo T.Type T.Type)	-- typeInst
	-> Map Var (T.Kind, Maybe T.Type)		-- typeQuantVars
	-> ProjTable					-- projection dictinary
	-> Map Var Var					-- how to resolve projections
	-> IO	( C.Tree
		, C.Tree )

toCore	sourceTree
	headerTree
	sigmaTable
	typeTable
	typeInst
	quantVars
	projTable
	projResolve
 = {-# SCC "toCore" #-} 
   do
	-----
	let toCoreTree'	
		= toCoreTree
			sigmaTable
			typeTable
			typeInst
			quantVars
			projTable
			projResolve
			
 	let cSource	= toCoreTree' sourceTree
	let cHeader	= toCoreTree' headerTree

	-----
	dumpCT DumpCore "core-source" cSource
	dumpCT DumpCore "core-header" cHeader

	
	return 	( cSource
		, cHeader )
	
