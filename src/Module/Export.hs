
module Module.Export
	(makeInterface)

where

-----
import qualified Data.Map		as Map
import Data.Map				(Map)

import qualified Data.Set		as Set
import Data.Set				(Set)

-----
import Util

import qualified Shared.Var		as Var
import Shared.Var			(Var, (=~=), Module)
import Shared.Pretty
import Shared.Error			(panic)
import Shared.Base			(SourcePos)
import Shared.Error

import qualified Module.Interface	as MI

import qualified Source.Exp		as S
import qualified Source.Horror		as S
import qualified Source.Pretty		as S
import qualified Source.Plate.Trans	as S
import qualified Source.Slurp		as S

import Type.Exp				(Type)
import qualified Type.Exp		as T
import qualified Type.Pretty		as T
import qualified Type.Util		as T
import qualified Type.Plate		as T

import qualified Desugar.Exp		as D
import qualified Desugar.Plate.Trans	as D
import qualified Desugar.Pretty		as D

import qualified Core.Exp		as C
import qualified Core.Pretty		as C
import qualified Core.Util		as C
import qualified Core.ToSea		as C

-----
stage	= "Module.Export"

-----
makeInterface 
	:: Module		-- name of this module
	-> S.Tree SourcePos	-- source tree
	-> D.Tree SourcePos	-- desugared tree
	-> C.Tree		-- core tree
	-> Map Var Var		-- sigma table
	-> Map Var T.Type	-- schemeTable
	-> Set Var		-- don't export these vars
	-> IO String
	
makeInterface
	moduleName
	sTree
	dTree
	cTree
	sigmaTable
	schemeTable
	vsNoExport

 = do
	-- To make the interfaces easier to read:
	--	For each var in the tree, if the var was bound in the current module
	--	then erase that module annotation.
	--	
	let sTree'	= eraseVarModuleTree moduleName sTree


	let getTVar 	= \v -> fromMaybe (panic stage $ "makeInterface: not found " ++ pprStrPlain v)
			$ Map.lookup v 
			$ sigmaTable

	let getType 	= \v -> fromMaybe (panic stage $ "makeInterface: not found " ++ pprStrPlain v)
			$ liftM (eraseVarModuleT moduleName)
			$ Map.lookup (getTVar v) schemeTable

	let topVars	= Set.fromList
			$ catMap S.slurpTopNames sTree

	return	$ exportAll getType topVars sTree' dTree cTree vsNoExport

-----
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

	
-----
exportAll 
	:: (Var -> Type)
	-> Set Var			-- vars in scope at top level
	-> [S.Top SourcePos] 
	-> [D.Top SourcePos]
	-> [C.Top]
	-> Set Var
	-> String

exportAll getType topNames ps psDesugared psCore vsNoExport
 	=  pprStrPlain
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
	++ (concat [pprStrPlain p | p@S.PData{}		<- ps])
	++ "\n"

	++ "-- Effects\n"
	++ (concat [pprStrPlain p | p@S.PEffect{}		<- ps])
	++ "\n"

	++ "-- Regions\n"
	++ (concat [pprStrPlain p | p@S.PRegion{}		<- ps])
	++ "\n"
	
	++ "-- Classes\n"
	++ (concat [pprStrPlain p | p@S.PClass{}		<- ps])
	++ "\n"
	
	++ "-- Class dictionaries\n"
	++ (concat [pprStrPlain p | p@S.PClassDict{}		<- ps])
	++ "\n"

	++ "-- Class instances\n"
	++ (concat [pprStrPlain p | 	p@D.PClassInst{}			
			     <- map (D.transformN (\n -> Nothing :: Maybe ()) ) psDesugared])
	++ "\n"
	
	++ "-- Foreign imports\n"
	++ (concat [pprStrPlain p 
			| p@(S.PForeign _ (S.OImport (S.OExtern{})))	<- ps])
	++ "\n"

	++ "-- Binds\n"
	-- only export types for bindings that were in scope in the source
	--	not lifted supers as well.

	++ (concat [exportVTT v (getType v) (C.superOpTypeX x)
			| p@(C.PBind v x) <- psCore
			, not $ Set.member v vsNoExport])		

--	++ "-- Projection dictionaries\n"
	++ (concat [exportProjDict p 	| p@D.PProjDict{}		
					<- psDesugared])

	++ "\n"

 
-----
exportVTT
	:: Var -> Type -> C.Type 
	-> String

exportVTT	v      tv to
	= pprStrPlain
	$ "foreign import extern "
	% pprStrPlain v { Var.nameModule = Var.ModuleNil }
	%>	(  "\n:: " ++ (pprStrPlain $ T.prettyTS $ T.normaliseT tv)
		++ "\n:$ " ++ pprStrPlain to

		++ ";\n\n")

-----
exportProjDict 
	:: D.Top a -> String

exportProjDict (D.PProjDict _ t [])
	= []

exportProjDict (D.PProjDict _ t ss)
 	= pprStrPlain
	$ "project " % t % " where\n"
	% "{\n"
	%> "\n" %!% (map (\(D.SBind _ (Just v1) (D.XVar _ v2)) 
			-> v1 %>> " = " % v2 { Var.nameModule = Var.ModuleNil } % ";") ss)
	% "\n}\n\n"
	
	



