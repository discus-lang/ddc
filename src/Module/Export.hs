
module Module.Export
	(makeInterface)

where

import qualified Source.Exp		as S
import qualified Source.Horror		as S
import qualified Source.Pretty		as S
import qualified Source.Plate.Trans	as S
import qualified Source.Slurp		as S
import qualified Source.Util		as S

import Type.Exp				(Type)
import qualified Type.Exp		as T
import qualified Type.Pretty		as T
import qualified Type.Util		as T
import qualified Type.Plate		as T
import qualified Type.Plug		as T

import qualified Desugar.Exp		as D
import qualified Desugar.Plate.Trans	as D
import qualified Desugar.Pretty		as D

import qualified Core.Exp		as C
import qualified Core.Pretty		as C
import qualified Core.Util		as C
import qualified Core.ToSea		as C

import qualified Shared.Var		as Var
import Shared.Var			(Var, (=~=), Module)
import Shared.VarPrim
import Shared.Pretty
import Shared.Error			(panic)
import Shared.Base			(SourcePos)
import Shared.Error

import Util

import qualified Data.Map		as Map
import Data.Map				(Map)

import qualified Data.Set		as Set
import Data.Set				(Set)

import Debug.Trace

--
stage	= "Module.Export"

-- Make a .di interface file for this module.
makeInterface 
	:: Module		-- name of this module
	-> S.Tree SourcePos	-- source tree
	-> D.Tree SourcePos	-- desugared tree
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
			= eraseVarModuleTree moduleName sTree

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
	let interface	= exportAll moduleName getType topVars sTree_erasedModules dTree cTree 
			$ shouldExport vsNoExport mExports

	return interface

-- Decide whether to export a particular var
shouldExport :: Set Var -> Maybe (Set Var) -> Var -> Bool
shouldExport vsNoExport mExports v
	| Set.member v vsNoExport	
	= False

	-- force projection functions to be exported 
	-- TODO: this is dodgey
	| take 7 (Var.name v) == "project"
	= True

	-- force instance functions to be exported 
	-- TODO: this is dodgey
	| take 8 (Var.name v) == "instance"
	= True
	
	| Just exports	<- mExports
	= Set.member v exports 

	| otherwise
	= True

-- Export all the top level things in this module
exportAll 
	:: Module
	-> (Var -> Type)	-- ^ a fn to get the type scheme of a top level binding
	-> Set Var		-- ^ vars of top level bindings.
	-> [S.Top SourcePos] 	-- ^ source tree
	-> [D.Top SourcePos]	-- ^ desugared tree
	-> [C.Top]		-- ^ core tree
	-> (Var -> Bool)	-- ^ don't export these vars
	-> String		-- ^ the interface file

exportAll moduleName getType topNames ps psDesugared_ psCore export
 = let	psDesugared	= map (D.transformN (\n -> Nothing :: Maybe ()) ) psDesugared_
   in   pprStrPlain
	$  "-- Imports\n"
	++ (concat [pprStrPlain p | p@S.PImportModule{} 	<- ps])
	++ "\n"

	++ "-- Pragmas\n"
	++ (concat [pprStrPlain p 
			| p@(S.PPragma _ (S.XVar sp v : _))	<- ps
			, Var.name v == "LinkObjs" ])

	++ "\n"

	++ "-- Infix\n"
	++ (concat [pprStrPlain p | p@S.PInfix{}		<- ps])
	++ "\n"

	++ "-- Data\n"
	++ (concat [pprStrPlain (D.PData sp (eraseModule vData) vsData (map eraseModule_ctor ctors))
			| D.PData sp vData vsData ctors
			<- psDesugared])
	++ "\n"

	++ "-- Effects\n"
	++ (concat [pprStrPlain p | p@S.PEffect{}		<- ps])
	++ "\n"

	++ "-- Regions\n"
	++ (concat [exportRegion moduleName p 
			| p@(C.PRegion r vts)		<- psCore])
	++ "\n\n"
	
	++ "-- Classes\n"
	++ (concat [pprStrPlain p 
			| p@S.PClass{}		<- ps])
	++ "\n"
	
	++ "-- Class dictionaries\n"
	++ (concat [pprStrPlain p 
			| p@D.PClassDict{}		<- psDesugared])
	++ "\n"

	++ "-- Class instances\n"
	++ (concat [pprStrPlain p 
			| p@D.PClassInst{}	<- psDesugared])
	++ "\n"
	
	++ "-- Foreign imports\n"
	++ (concat [pprStrPlain p 
			| p@(S.PForeign _ S.OImport{})			<- ps])

	++ (concat [pprStrPlain p 
			| p@(S.PForeign _ S.OImportUnboxedData {})	<- ps])

	++ "\n"

	-- only export types for bindings that were in scope in the source, and
	--	not lifted supers as well as they're not needed by the client module.
	++ "-- Binds\n"
	++ (concat [exportForeign v (getType v) (C.superOpTypeX x)
			| p@(C.PBind v x) <- psCore
			, export (eraseVarModuleV moduleName v)])		

--	++ "-- Projection dictionaries\n"
	++ (concat [exportProjDict p 	| p@D.PProjDict{}		
					<- psDesugared])

	++ "\n"


-- | Erase a the module name from this var 
eraseModule :: Var -> Var
eraseModule v
	= v { Var.nameModule = Var.ModuleNil }


eraseModule_ctor (D.CtorDef sp v fs)
	= D.CtorDef sp (eraseModule v) fs


 
-- | make a foreign import to import this scheme
exportForeign
	:: Var 		-- var of the binding
	-> Type 	-- type of the binding
	-> C.Type 	-- operational type of the binding
	-> String	-- import text

exportForeign v tv to
	= pprStrPlain
	$ "foreign import "
	% pprStrPlain v { Var.nameModule = Var.ModuleNil }
	%>	(  "\n:: " ++ (pprStrPlain $ T.prettyTS $ T.normaliseT tv)
		++ "\n:$ " ++ pprStrPlain to

		++ ";\n\n")


-- | export  a projection dictionary
exportProjDict :: D.Top a -> String
exportProjDict (D.PProjDict _ t [])
	= []

exportProjDict (D.PProjDict _ t ss)
 	= pprStrPlain
	$ "project " % t % " where\n"
	% "{\n"
	%> "\n" %!% (map (\(D.SBind _ (Just v1) (D.XVar _ v2)) 
			-> v1 %>> " = " % v2 { Var.nameModule = Var.ModuleNil } % ";") ss)
	% "\n}\n\n"
	
-- | export a top level region decl
exportRegion :: Module -> C.Top -> String
exportRegion mod (C.PRegion r vts)
	| Var.nameModule r == Var.ModuleNil
	= pprStrPlain
	$ "region " % mod % "." % r % ";" % "\n"
{-	% (case vts of 
		[]	-> pNil
		_	-> " :- " % ", " %!% [t | (v, t) <- vts ])
-}

	| otherwise
	= ""

--	= pprStrPlain
--	$ "region " % Var.nameModule r % "." % r % ";"

-- | erase module qualifiers from variables in this tree
eraseVarModuleTree
	:: Module
	-> S.Tree SourcePos
	-> S.Tree SourcePos
	
eraseVarModuleTree
	m tree
 =	S.trans (S.transTableId (\(x :: SourcePos) -> return x))
		{ S.transVar	= \v -> return $ eraseVarModuleV m v 
		, S.transType	= \t -> return $ T.transformV (eraseVarModuleV m) t 
		}
		tree	


eraseVarModuleT m t
 	= T.transformV (eraseVarModuleV m) t

eraseVarModuleV m v
 = if Var.nameModule v == m
 	then v { Var.nameModule = Var.ModuleNil }
	else v

