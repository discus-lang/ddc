{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Module.Export
	(makeInterface)
where
import Util
import DDC.Type.Data.Pretty
import DDC.Base.SourcePos
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Var
import DDC.Desugar.Pretty		()
import DDC.Type				(Type)
import Source.Desugar			(Annot)
import Source.Pretty			()
import qualified Data.Map		as Map
import qualified Data.Set		as Set
import qualified Source.Exp		as S
import qualified Source.Plate.Trans	as S
import qualified Source.Slurp		as S
import qualified Source.Util		as S
import qualified DDC.Type		as T
import qualified DDC.Type.Data		as T
import qualified DDC.Type.Transform	as T
import qualified DDC.Desugar.Exp	as D
import qualified DDC.Desugar.Transform	as D
import qualified DDC.Core.OpType	as C
import qualified DDC.Core.Exp		as C

stage	= "DDC.Module.Export"

-- Make a .di interface file for this module.
makeInterface
	:: ModuleId		-- name of this module
	-> S.Tree SourcePos	-- source tree
	-> D.Tree Annot		-- desugared tree
	-> C.Tree		-- core tree
	-> Map Var Var		-- sigma table (value var -> type var)
	-> Map Var T.Type	-- schemeTable
	-> Set Var		-- don't export these vars under any circumustances
				--	(eg, lifted super combinators aren't useful for anyone else)
	-> IO String

makeInterface moduleName
	sTree dTree cTree
	sigmaTable
	schemeTable
	vsNoExport
 = do
	-- For each variable in the tree, if it is bound in the current module then erase its
	--	module id. This makes the interfaces easier to read, and lets as parse
	--	the interface files as if they were source code.
	let sTree_erasedModules
			= eraseVarModuleSourceTree moduleName sTree

	let dTree_erasedModules
			= eraseVarModuleDesugaredTree moduleName dTree

	-- a fn to lookup the type var for a value var
	let getTVar 	= \v -> fromMaybe (panic stage $ "makeInterface: not found " ++ pprStrPlain v)
			$ Map.lookup v $ sigmaTable

	-- a fn to get a top level type scheme
	let getType 	= \v -> fromMaybe (panic stage $ "makeInterface: not found " ++ pprStrPlain v)
			$ liftM (eraseVarModuleT moduleName)
			$ Map.lookup (getTVar v) schemeTable

	-- the vars of all the top level things
	let topVars	= Set.fromList $ catMap S.slurpTopNames sTree_erasedModules

	-- see if there is an explicit export list
	let mExports	= case [exs | S.PExport _ exs <- sTree_erasedModules] of
				[]	-> Nothing
				ee	-> Just $ Set.fromList
						$ map S.takeExportVar
						$ concat ee

	-- export all the top level things
	let interface	= exportAll moduleName getType topVars sTree_erasedModules dTree_erasedModules cTree
			$ shouldExport vsNoExport mExports

	return interface

-- Decide whether to export a particular var
shouldExport :: Set Var -> Maybe (Set Var) -> Var -> Bool
shouldExport vsNoExport mExports v
	| Set.member v vsNoExport
	= False

	-- force projection functions to be exported
	-- TODO: this is dodgey
	| take 7 (varName v) == "project"
	= True

	-- force instance functions to be exported
	-- TODO: this is dodgey
	| take 8 (varName v) == "instance"
	= True

	| Just exports	<- mExports
	= Set.member v exports

	| otherwise
	= True


-- | Export all the top level things in this module
--   TODO: This is an epic hack that relies on some of the pretty printed desugared and core
--         constructs being the same as the source. This interface file format is being replaced
--         by the saner version in Module.Interface.
exportAll
	:: ModuleId
	-> (Var -> Type)	-- ^ a fn to get the type scheme of a top level binding
	-> Set Var		-- ^ vars of top level bindings.
	-> [S.Top SourcePos] 	-- ^ source tree
	-> [D.Top Annot]	-- ^ desugared tree
	-> [C.Top]		-- ^ core tree
	-> (Var -> Bool)	-- ^ don't export these vars
	-> String		-- ^ the interface file

exportAll moduleName getType _ psSource psDesugared_ psCore export
 = let	psDesugared	= map (D.transformN (\_ -> Nothing :: Maybe ()) ) psDesugared_
   in   pprStrPlain
	$ vcat
	[ ppr "-- Pragmas"
	, vcat	[ppr p 	| p@(S.PPragma _ (S.XVar _ v : _)) <- psSource
			, varName v == "LinkObjs" ]
	, blank

	, ppr "-- Module Imports"
	, vcat	[ppr p	| p@S.PImportModule{} 	<- psSource]
	, blank


	, ppr "-- Infix Operators"
	, vcat	[ppr p	| p@S.PInfix{}		<- psSource]
	, blank

	, ppr "-- Abstract data types"
 	, vcat	[ ppr (D.PKindSig ann (eraseModule vData) k)
			| D.PKindSig ann vData k <- psDesugared
			, T.resultKind k == T.kValue ]

	, ppr "-- Data Types"
	, vcat	[ (pprDataDefAsSource
			$ T.DataDef
				(eraseModule vData)
				vSea
				([(eraseModule v, k) | (v, k) <- vksData])
				(Map.map eraseModule_ctor ctors)
				vsMaterial
				vsImmaterial)
		    % ";"
			| D.PData _ (T.DataDef vData vSea vksData ctors vsMaterial vsImmaterial)
			<- psDesugared]
	, blank

	, ppr "-- Effects"
	, vcat	[ppr p	| p@(S.PKindSig _ _ k)	<- psSource
			, T.resultKind k == T.kEffect ]
	, blank

	, ppr"-- Regions"
	, vcat	[ exportRegion moduleName p
			| p@C.PRegion{}		<- psCore]
	, blank

	, ppr "-- Abstract Type Classes"
	, vcat	[ ppr p	|  p@S.PClass{}		<- psSource]
	, blank

	, ppr "-- Type Class Declarations"
	, vsep	[ ppr p | p@D.PClassDecl{} 	<- psDesugared]
	, blank

	, ppr "-- Type Class Instances"
	, vsep	[ ppr p | p@D.PClassInst{}	<- psDesugared]
	, blank

	, ppr "-- Projection dictionaries"
	, vsep	[ exportProjDict p
			| p@D.PProjDict{}	<- psDesugared]
	, blank

	, ppr "-- Foreign imports"
	, vsep [ ppr p 	| p@D.PExtern{}		<- psDesugared]
	, blank

	-- only export types for bindings that were in scope in the source, and
	--	not lifted supers as well as they're not needed by the client module.
 	, ppr "-- Binding Types"
	, vsep	[ exportForeign v (getType v) (C.superOpTypeX x)
			| C.PBind v x <- psCore
			, export (eraseVarModuleV moduleName v)]
	]

-- | Erase a the module name from this var
eraseModule :: Var -> Var
eraseModule v	= v { varModuleId = ModuleIdNil }

eraseModule_ctor def@T.CtorDef{}
	= def { T.ctorDefName	= eraseModule (T.ctorDefName def) }



-- | make a foreign import to import this scheme
exportForeign
	:: Var 		-- var of the binding
	-> Type 	-- type of the binding
	-> Type 	-- operational type of the binding
	-> String	-- import text

exportForeign v tv to
	= pprStrPlain
	$ "foreign import "
	% pprStrPlain v { varModuleId = ModuleIdNil } % nl
	%> vcat	[ "::" %% (T.prettyTypeSplit $ T.beautifyLocalNamesT tv)
		, ":$" %% to %% semi ]


-- | export  a projection dictionary
exportProjDict :: D.Top a -> Str
exportProjDict (D.PProjDict _ t ss@(_:_))
 	= pprHeadBlock ("project " % t % " where" % nl)
	[ v1 %>> " = " % v2 { varModuleId = ModuleIdNil }
		| (D.SBind _ (Just v1) (D.XVar _ v2))	<- ss ]

exportProjDict _ = blank


-- | export a top level region decl
exportRegion :: ModuleId -> C.Top -> Str
exportRegion m (C.PRegion r _)
	| varModuleId r == ModuleIdNil
	= "region " % m % "." % r % ";" % nl

exportRegion _ _ = blank



-- | erase module qualifiers from variables in this tree
eraseVarModuleSourceTree
	:: ModuleId
	-> S.Tree SourcePos
	-> S.Tree SourcePos

eraseVarModuleSourceTree m tree
 =	S.trans (S.transTableId (\(x :: SourcePos) -> return x))
		{ S.transVar	= \v -> return $ eraseVarModuleV m v
		, S.transType	= \t -> return $ T.transformV (eraseVarModuleV m) t
		}
		tree

-- | erase module qualifiers from variables in this tree
eraseVarModuleDesugaredTree
	:: ModuleId
	-> D.Tree Annot
	-> D.Tree Annot

eraseVarModuleDesugaredTree m tree
 =	map 	(D.transZ (D.transTableId (\(x :: Annot) -> return x))
			{ D.transV	= \v -> return $ eraseVarModuleV m v
			, D.transT	= \t -> return $ T.transformV (eraseVarModuleV m) t  })
		tree

eraseVarModuleT m t
 	= T.transformV (eraseVarModuleV m) t


eraseVarModuleV m v
 = if varModuleId v == m
 	then v { varModuleId = ModuleIdNil }
	else v

