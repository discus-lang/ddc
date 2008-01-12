
module Core.Optimise.FullLaziness
(
	fullLazinessTree
)


where

import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Shared.Var	as Var
import Shared.Var		(NameSpace(..), Module, VarBind)
import qualified Shared.VarBind	as Var
import qualified Shared.Unique	as Unique

import Core.Exp
import Core.Util
import Core.Util.Slurp

import Core.Inline
import Core.Snip
import Core.Reconstruct
import Core.Plate.FreeVars

import qualified Core.Plate.Trans	as Trans

import Core.Optimise.FreeLevel

-----
stage	= "Core.Optimise.FullLaziniess"

uniqueLift n	= "x" ++ show n ++ "l" 	++ Unique.coreFullLaziness
unqiueSnip 	= "x" ++ Unique.coreFullLaziness


type FullM	= State ()


fullLazinessTree 
	:: Module	-- name of current module
	-> Tree		-- header tree	
	-> Tree		-- core tree
	-> Tree

fullLazinessTree
	moduleName
	cHeader
	cTree 
 = let
	treeInlined		= inlinePureSingleTree cTree

 	treeLevel		= annotLevelTree cHeader treeInlined
	(cafsVX0, treeSlurp0)	= slurpLevelTree 0 treeLevel

	cafsP0			= map (\(SBind (Just v) x) -> PBind v (XDo [SBind Nothing x])) 
				$ cafsVX0

	treeFull		= cafsP0 ++ treeSlurp0
	treeMod			= moduleifyTree moduleName treeFull

	treeErase		= eraseAnnotsTree treeMod
	treeSnip		= snipTree Set.empty unqiueSnip treeErase
	treeRecon		= reconTree stage cHeader treeSnip

     in	treeRecon

-----
-- moduleifyTree
--	After lambda lifting we will have created a set of new top
--	level bindings. These new bindings won't have module information on them, and 
--	will conflict with names from other modules if we don't add it.
--

moduleifyTree
	:: Module
	-> Tree	-> Tree
	
moduleifyTree 
	moduleName
	tree
 = let
 	topVs	= catMap slurpBoundVarsP tree
	subVs	= map (\v -> (v, v { Var.nameModule = moduleName })) topVs
	
	tree'	= Trans.transformV 
			(\v -> case lookup v subVs of
				Nothing	-> v
				Just v'	-> v')
			tree
   in	tree'	


-----
slurpZeroSS :: [Stmt] -> State [Stmt] [Stmt]
slurpZeroSS ss
 = do	ss'	<- mapM slurpZeroS ss
 	return	$ catMaybes ss'

slurpZeroS ss
 = case ss of
 	SBind (Just v) x@(XAnnot [NLevel 0] z)
	 |  canLiftX z 
	 && slurpEffsX z == pure
	 -> do 	modify (\s -> s ++ [ss])
	 	return Nothing
		
	_ ->	return $ Just ss


canLiftX :: Exp -> Bool
canLiftX xx
 = case xx of
	XTau t x			-> canLiftX x
	XAnnot n x			-> canLiftX x

 	XPrim (MBox tB tU) x		-> True
	
	XApp{}				-> True	
	XAPP{}				-> True	

	_				-> False



-----
type	SlurpS	= ([Stmt], VarBind)
type	SlurpM	= State SlurpS

slurpLevelTree 
	:: Int 
	-> Tree 
	-> ( [Stmt]	-- lifted bindings
	    ,Tree)	-- new tree
	
slurpLevelTree n tree
 = let
 	(tree', (stmts, gen'))
		= runState
			(Trans.transZM
				Trans.transTableId
				{ Trans.transX_enter	= slurpLevelX n }
				tree)
			([], Var.XBind (uniqueLift n) 0)
			
   in	(stmts, tree')
						


slurpLevelX ::	Int -> Exp -> SlurpM Exp
slurpLevelX n xx
 = case xx of
 	XAnnot [NFreeLevel []] x
	 |  slurpEffsX x == pure
	 ,  canLiftX x
	 -> do
	 	(ss, gen)	<- get
		
		let var		= (Var.new (pretty gen))
				{ Var.bind 	= gen 
				, Var.nameSpace	= NameValue}
			
		let s		= SBind (Just var) x
		let gen'	= Var.incVarBind gen
		
		put	(ss ++ [s], gen')

		return	$ XVar var TNil
	 
	_ -> return xx
	
	
	
 








