-----
-- Stages.Source
--
--	BUGS: sourceSlurpInline vars should check that vars to inline are in scope at top level
--

module Stages.Source
	( Stages.Source.parse
	, sourcePragma
	, sourceSlurpFixTable
	, sourceSlurpInlineVars
	, defix
	, rename
	, sourceKinds
	, desugar
	, desugarProject
	, desugarSnipLambda
	, alias
	, slurpC
	, solveSquid
	, toCore )

where


-----
import qualified Util.Map		as Map

import qualified Data.Map		as Map
import Data.Map (Map)

import qualified Data.Set		as Set
import Data.Set	(Set)

import System.Exit
import System.IO

import Util

-----
import Shared.Error			(panic)

import qualified Shared.Var		as Var
import Shared.Var			(Var, Module)
import Shared.Base
import Shared.Pretty
import Shared.Error

-----
import qualified Type.Exp 			as T
import qualified Type.Pretty			as T
import qualified Type.ToCore			as T
import qualified Type.Error			as T
import qualified Type.Util			as T
import qualified Type.Solve			as Squid
import qualified Type.State			as Squid
import qualified Type.Export			as Squid
import qualified Type.Dump			as Squid

import qualified Constraint.Exp			as N

import qualified Source.Token			as Token
import qualified Source.Rename			as S
import qualified Source.RenameM			as S
import qualified Source.Lint			as S
import qualified Source.RenameM			as Rename
import Source.Lexer				(scan)
import Source.Parser				(parse)
import Source.Slurp				(slurpFixTable, slurpKinds)
import Source.Defix				(defixP)
import Source.Desugar				(rewriteTree)
import Source.Alias				(aliasTree)
import Source.RenameM				(runRename, RenameM)
import Source.Pretty
import Source.Exp
import Source.Horror

import qualified Desugar.Exp			as D
import qualified Desugar.Util			as D
import qualified Desugar.Slurp.State		as D
import qualified Desugar.Pretty			as D
import qualified Desugar.Plate.Trans		as D
import Desugar.Slurp.Slurp			(slurpTreeM)
import Desugar.Project				(projectTree, ProjTable, slurpProjTable)
import Desugar.SnipLambda			(snipLambdaTree)
import Desugar.ToCore				(toCoreTree)

import qualified Core.Exp			as C
import qualified Core.Util			as C
import qualified Core.Pretty			as C

import Main.Arg
import Main.Path

import Stages.Dump

-----
stage = "Stages.Source"

-----------------------
-- parse
--
parse 	:: (?args :: [Arg])
	-> FilePath			-- path of source file
	-> String			-- source of root module
	-> IO Tree			-- source parse tree
			
parse	fileName
	source
 = do
	let tokens	= map (\t -> t { Token.file = fileName }) 
			$ scan source
			
	let sParsed	= Source.Parser.parse tokens
	
	-- dump
	dumpST 	DumpSourceParsed "source-parsed" sParsed

	return	sParsed

-----------------------
-- sourcePragma
--
sourcePragma
	:: (?args :: [Arg])
	-> Tree
	-> IO 	( [String]		-- Shell commands to run
		, [String])		-- Extra objects to link with
	
sourcePragma tree
 = do
 	let shellCommands	
		= [str	| XConst sp (CConst (LString str)) <- concat
				[strings | PPragma [XVar sp v, XList _ strings] <- tree
					 , Var.name v == "Shell" ] ]

	return	(shellCommands, [])


-----------------------
-- slurpH
--
sourceSlurpFixTable
	:: Tree				-- source and header parse tree
	-> IO [FixDef]			-- fixity table
		
sourceSlurpFixTable
	sTree
 = do
	let fixTable	= slurpFixTable sTree
	return	fixTable


-----------------------
-- sourceSlurpInlineVars
-- 
sourceSlurpInlineVars
	:: Tree
	-> IO [Var]
	
sourceSlurpInlineVars 
	sTree
 = 
 	return	$ catMap
			(\p -> case p of
 				PPragma [XVar sp v, XList _ xs]
				 | Var.name v == "Inline"
				 , vs		 <- map (\(XVar sp v) -> v) xs
				 -> vs
			 
				_ -> [])
			sTree

-----------------------
-- defix
--
defix	:: (?args :: [Arg])
	-> Tree				-- source parse tree
	-> [FixDef]			-- fixity table
	-> IO Tree			-- defixed parse tree, will have no more XInfix nodes.
	
defix	sParsed
	fixTable
 = do
 	let (sDefixed, errss)
			= unzip
			$ map (defixP fixTable)
			$ sParsed
	
	handleErrors 	$ concat errss
	
	-- dump
	dumpST	DumpSourceDefixed "source-defixed" sDefixed
	
	return	 sDefixed
	

-----------------------
-- rename
--
-- NOTES:
-- 	We need to rename infix defs _after_ foreign imports
--	We do this so that the Sea name for functions like (+ / primInt32Add)
--	which is present on foreign decls gets propagated to uses of these functions.
--
rename	:: (?args :: [Arg])
	-> 	[(Module, Tree)]
	-> IO 	[(Module, Tree)]


rename	mTrees
 = do
	let (mTrees', state')
		= runState (S.renameTrees mTrees)
		$ S.initRenameS

	handleErrors (S.stateErrors state')

	-- dump
	let Just sTree	= liftM snd $ takeHead mTrees'

	dumpST 	DumpSourceRenamed "source-renamed" sTree
	dumpST 	DumpSourceRenamed "header-renamed" (concat $ map snd $ tail mTrees')
	
	return mTrees'


-----------------------
-- sourceKinds
--
sourceKinds
	:: (?args :: [Arg])
	-> Tree
	-> IO [(Var, Kind)]
	
sourceKinds sTree
 = do	let kinds	= slurpKinds sTree
	dumpS DumpSourceKinds "source-kinds" 
		(catMap (\(v, k) -> pretty $ v %>> " :: " % k % ";\n") kinds)
		
	return	kinds


-----------------------
-- alias
--
alias 	:: (?args :: [Arg])
	-> Tree
	-> IO Tree
	
alias sTree
 = do
 	let sTree'	= aliasTree sTree

	dumpST	DumpSourceAliased "source-aliased" sTree'

	return	sTree'
	
	
-----------------------
-- desugar
--
desugar
	:: (?args :: [Arg])
	-> [(Var, Kind)]		-- kind table
	-> Tree				-- source tree
	-> Tree				-- header tree
	-> IO 	( D.Tree SourcePos
		, D.Tree SourcePos)
	
desugar	kinds sTree hTree
 = do
	let kindMap	= Map.fromList kinds
	let sTree'	= rewriteTree kindMap sTree
	let hTree'	= rewriteTree kindMap hTree
			
	-- dump
	dumpST DumpDesugared "desugared-source" 
		(map (D.transformN $ \a -> (Nothing :: Maybe ())) sTree')

	dumpST DumpDesugared "desugared-header" 
		(map (D.transformN $ \a -> (Nothing :: Maybe ())) hTree')
		
	return	(sTree', hTree')

-----------------------
-- desugarSnipLambda
--
desugarSnipLambda
	:: (?args :: [Arg])
	-> String
	-> String
	-> D.Tree SourcePos
	-> IO	(D.Tree SourcePos)
	
desugarSnipLambda name unique tree
 = do	let tree'	= snipLambdaTree unique tree
 	
	-- 
	dumpST DumpDesugared name
		(map (D.transformN $ \a -> (Nothing :: Maybe ())) tree')
		
	return tree'
	
	
-----------------------
-- desugarProject
-- 
desugarProject 
	:: (?args :: [Arg])
	-> D.Tree SourcePos
	-> D.Tree SourcePos
	-> IO	( D.Tree SourcePos
		, ProjTable )

desugarProject headerTree sourceTree
 = do
	-- Snip down projection dictionaries and add default projections.
 	let sourceTree'	= projectTree headerTree sourceTree 
	
	dumpST DumpDesugaredProject "desugared-project"
		(map (D.transformN $ \a -> (Nothing :: Maybe ())) sourceTree')

	-- Slurp out the ProjTable
	let projTable	= slurpProjTable (headerTree ++ sourceTree')

	dumpS  DumpDesugaredProject "desugared-project-dicts"
		(pretty $ "\n" %!% (map prettyp $ Map.toList projTable))

		
	return (sourceTree', projTable)

	
	
-----------------------
-- slurpC
--
slurpC 	:: (?args :: [Arg])
	-> (D.Tree SourcePos)				-- source tree
	-> (D.Tree SourcePos)				-- header tree
	-> IO	( (D.Tree (Maybe (Type, Effect)))	-- source tree with type and effect annotations
		, [N.CTree]				-- type constraints
		, Map Var Var 				-- sigma table (map of value -> type vars)
		, Set Var)				-- all the value vars used in the program that we'll want types for
		
slurpC	sTree
	hTree

 = do
	let state	= D.initCSlurpS 
	
	let ((header', hctrs), state2)
			= runState (slurpTreeM hTree)
			$ state
		
	let ((source', sctrs), state3)
			= runState (slurpTreeM sTree)
			$ state2

	handleErrors (D.stateErrors state3)

	let constraints	= hctrs ++ sctrs
	let sigmaTable	= D.stateVarType   state3

	let vsTypesPlease 	= D.stateTypesPlease state3

	-- dump
	dumpST	DumpDesugaredSlurped "desugared-slurped" source'
	
	dumpS	DumpTypeConstraints "type-constraints--source"
		$ (catInt "\n" $ map pretty sctrs)
		
	dumpS	DumpTypeConstraints "type-constraints--header"
		$ (catInt "\n" $ map pretty hctrs)

	dumpS	DumpTypeConstraints "type-constraints--typesPlease"
		$ (catInt "\n" $ map show $ Set.toList vsTypesPlease)
		
	dumpS	DumpTypeSlurp  "type-slurp-trace"
		$ concat $ D.stateTrace state3
	--
	return	( source'
		, constraints
		, sigmaTable 
		, vsTypesPlease)
	
	
-----
solveSquid :: (?args :: [Arg])
	-> [N.CTree]				-- type constraints
	-> Set Var				-- the TEC vars to infer TECs for	
	-> Map Var Var				-- sigma table
	
	-> IO 	( Map Var T.Type			-- inferred types
		, Map Var (InstanceInfo T.Type T.Type))	-- how each var was instantiated
	
solveSquid 
	constraints
	vsTypesPlease
	sigmaTable

 = do
	-- The solver state gets dumped in real-time so we can see
	--	what's gone wrong if it crashes mid-stream.

	hTrace	<- dumpOpen DumpTypeSolve "type-solve--trace"
		
 	state	<- Squid.squidSolve 
			?args
			constraints
			sigmaTable
			hTrace

	-- flush the trace output to make sure it's written to the file.
	(case hTrace of
	 Just handle	-> hFlush handle
	 Nothing	-> return ())

	-- extract out the stuff we'll need for conversion to core.
	(typeTable, typeInst)
		<- evalStateT (Squid.squidExport vsTypesPlease) state

	-- dump final solver state
	dGraph	<- evalStateT Squid.dumpGraph state
	dumpS	DumpTypeSolve  "type-solve--graph" dGraph
	
	dumpS	DumpTypeSolve  "type-solve--types"
		$ catInt "\n\n"
		$ map pretty
		$ map (\(v, t) -> v % " ::\n" %> T.prettyTS t)
		$ Map.toList
		$ typeTable


	dumpS 	DumpTypeSolve   "type-solve--inst" 
		$ catInt "\n\n"
		$ map pretty
		$ Map.toList
		$ typeInst


	-- stop the compiler and print out errors
	--	if there were any during type inference.
	handleErrors 
		$ Squid.stateErrors state



{-
	-----
	let dSchemes	
		= catMap (\(v, t) -> pretty $ v % "\n" %> (":: " % T.prettyTS t) % "\n\n") 
		$ Map.toList schemeTable

	dumpS DumpTypeSchemes "type-schemes" dSchemes
-}

	-----
{-	let dCoreSchemes
		= catMap (\(v, t) -> pretty $ v % "\n" % " ::     " %> t % "\n\n")
		$ Map.toList coreSchemes
		
	dumpS DumpCoreSchemes "core-schemes" dCoreSchemes
-}		
	-----
	return 	( typeTable
		, typeInst)



-----------------------
-- toCore
--
toCore 	:: (?args :: [Arg])
	-> D.Tree (Maybe (Type, Effect))		-- sourceTree
	-> D.Tree (Maybe (Type, Effect))		-- headerTree
	-> (Map Var Var)				-- sigmaTable
	-> (Map Var T.Type)				-- typeTable
	-> (Map Var (T.InstanceInfo T.Type T.Type))		-- typeInst
	-> ProjTable
	-> IO	( C.Tree
		, C.Tree )

toCore	sourceTree
	headerTree
	sigmaTable
	typeTable
	typeInst
	projTable
 = do


	-----
	let toCoreTree'	
		= toCoreTree
			sigmaTable
			typeTable
			typeInst
			projTable

			
 	let cSource	= toCoreTree' sourceTree
	let cHeader	= toCoreTree' headerTree

	-----
	dumpCT DumpCore "core-source" cSource
	dumpCT DumpCore "core-header" cHeader

	
	return 	( cSource
		, cHeader )
	


-----
handleErrors 
	:: Pretty a
	=> (?args :: [Arg])
	-> [a] -> IO ()
		
handleErrors []
	= return ()
		
handleErrors errs
 = case filter (=@= StopErrors{}) ?args of
  	(StopErrors [file] : _)
	 -> do 	writeFile file 
			(catInt "\n" $ map pretty errs)
			
		exitWith ExitSuccess
		
	_ -> 	dieWithUserError errs
			

	 	



