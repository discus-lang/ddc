
module Source.Desugar.Dict
	( 
--	snipProjDictTree 
--	, addProjDictDataTree
--	, addProjDictFunsTree 
	)
where

import Util
import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Shared.Var	as Var
import Shared.Var		(Var, NameSpace(..))

import Type.Exp
import Shared.Exp

import Desugar.Exp
import Desugar.Util
import Source.Desugar.Base

{-
-----
-- snipProjDictTree
--	Snip out functions and sigs from projection dictionaries to
--	top level.
--
snipProjDictTree :: Tree a -> RewriteM (Tree a)
snipProjDictTree   tree
 	= liftM concat
 	$ mapM snipProjDictP tree
	
	
snipProjDictP (PProjDict nn t ss)
 = do
	-- See what vars are in the dict
	--	and make a map of new vars.
	--	
 	let dictVs	= nub
			$ catMaybes 
			$ map takeStmtBoundV ss
			
	dictVsNew 	<- mapM (\v -> newVarNS NameValue ("_" ++ pretty v)) dictVs
	let varMap	= Map.fromList $ zip dictVs dictVsNew
	
	-- 
	let (pp, mss')	= unzip $ map (snipProjDictS varMap) ss
	
	return	$ PProjDict nn t (catMaybes mss')
		: pp

snipProjDictP pp
 =	return [pp]


snipProjDictS :: Map Var Var -> Stmt a -> (Top a, Maybe (Stmt a))
snipProjDictS varMap xx
	| SBind nn (Just v) x	<- xx
	, Just v'		<- Map.lookup v varMap
	= ( PBind nn (Just v') x
	  , Just $ SBind nn (Just v)  (XVar nn v'))
	  	
	| SSig  nn v  t		<- xx
	, Just v'		<- Map.lookup v varMap
	= ( PSig  nn v'  t
	  , Nothing )
	

-----
-- addProjDictDataTree
--	Make sure there is a projection dictionary for each data type.
--	If one doesn't exist, then make a new one. 
--	This dictionary will be filled with the default projections by 
--	addProjDictFunTree.
--
addProjDictDataTree
 ::	Tree Annot
 -> 	RewriteM (Tree Annot)
 
addProjDictDataTree tree
 = do
	-- Slurp out all the data defs
	let dataDefs	= [p		| p@(PData _ v vs ctors)	<- tree]

 	-- Slurp out all the available projection dictionaries.
	let projMap	= Map.fromList
			$ [(v, p)	| p@(PProjDict _ t@(TCon v ts) ss)	<- tree]
		
	-- If there is no projection dictionary for a given data type
	--	then make a new empty one. Add new dictionary straight after the data def
	--	to make life easier for the type inference constraint reordering.
	-- 
	let tree'	= catMap (addProjDataP projMap) tree

	return	$ tree'
	

addProjDataP projMap p
 = case p of
	PData _ v vs ctors
 	 -> case Map.lookup v projMap of
		Nothing	-> [p, PProjDict none (TCon v (map TVar vs)) []]
		Just _	-> [p]
		
	_		-> [p]


-----
-- addProjDictFunsTree
--	Add default field projections to dictionaries for data types.
--
addProjDictFunsTree 
 :: 	Map Var (Top Annot) -> Tree Annot
 -> 	RewriteM (Tree Annot)

addProjDictFunsTree dataMap tree
 	= mapM (addProjDictFunsP dataMap) tree
	
addProjDictFunsP 
	dataMap 
	p@(PProjDict nn t@(TCon v ts) ss)
 = do
	-- Lookup the data def for this type.
 	let (Just (PData _ _ _ ctors))	
		= Map.lookup v dataMap
	
	-- See what projections have already been defined.
	let dictVs	= nub 
			$ catMaybes
			$ map takeStmtBoundV ss
	
	-- Gather the list of all fields in the data def.
	let dataFieldVs
			= nub
			$ [ fieldV 	| CtorDef _ v fields	<- ctors
					, Just fieldV		<- map dLabel fields ]			

	-- Only add a projection function if there isn't one
	--	for that field already in the dictionary.
	let newFieldVs
		= dataFieldVs \\ dictVs

	projFunsSS	
		<- mapM (makeProjFun ctors) newFieldVs
		
	return 	$ PProjDict nn t (projFunsSS ++ ss)
 
addProjDictFunsP dataMap p
 =	return p


makeProjFun 
 :: 	[CtorDef Annot] -> Var 
 ->	RewriteM (Stmt Annot)

makeProjFun    ctors fieldV
  = do 	alts	<- liftM catMaybes
  		$  mapM (makeProjFunAlt fieldV) ctors

	objV	<- newVarN NameValue
	
    	return	$ SBind none (Just fieldV) 
 			(XLambda none objV 
				(XMatch none (Just (XVar none objV)) alts))
				
makeProjFunAlt fieldV (CtorDef _ vCon fields)
 = do
 	let fieldCount	= length fields
	fieldVs		<- replicateM fieldCount (newVarN NameValue)

	let fieldV'	= lookup (Just fieldV)
			$ zip (map dLabel fields) fieldVs
			
	return	
	 $ case fieldV' of
	 	Just v	-> Just 
			$  AAlt none 
				[GCase none (WCon none vCon fieldVs)] 
				(XVar none v)

		Nothing	-> Nothing

-}
