-- Desugar.Project
--	Desugar projection dictionaries.
--
--	* For each data type, add a default projection dict if none exists.
--	* Add default projections.
--	* Snip user provided projection functions to top level.
--

module Desugar.Project
	( projectTree 

	, ProjTable
	, slurpProjTable )
where

import Util
import qualified Util.Map	as Map
import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Shared.Var	as Var
import Shared.Var		(Var, NameSpace(..))


import Type.Exp
import Type.Util
import Shared.Exp
import Shared.VarUtil
import Shared.Base
import Shared.Literal
import Shared.VarPrim
import Shared.Error

import qualified Shared.VarBind	as Var

import Desugar.Exp
import Desugar.Util

-----
stage	= "Desugar.Project"

-----
type	ProjectM	= VarGenM
type	Annot		= SourcePos
none			= NoSourcePos

projectTree :: Tree Annot -> Tree Annot -> Tree Annot
projectTree headerTree tree
	= evalState (projectTreeM headerTree tree) (Var.XBind "xDict" 0)
		
	
projectTreeM :: Tree Annot -> Tree Annot -> ProjectM (Tree Annot)
projectTreeM headerTree tree
 = do
	-- Slurp out all the data defs
	let dataMap	= Map.fromList
			$ [(v, p) 	| p@(PData _ v vs ctors) <- tree ++ headerTree]

	-- Each data type in the source file should have a projection dictionary
	--	if none exists then make a new empty one.
	treeProjNewDict	<- addProjDictDataTree tree

	-- Add default projections to projection dictionaries.
	treeProjFuns	<- addProjDictFunsTree dataMap treeProjNewDict
	
	-- Snip user functions out of projection dictionaries.
 	treeProjDict	<- snipProjDictTree treeProjFuns

	return treeProjDict


-----
-- snipProjDictTree
--	Snip out functions and sigs from projection dictionaries to
--	top level.
--
snipProjDictTree :: Tree a -> ProjectM (Tree a)
snipProjDictTree   tree
 	= liftM concat
 	$ mapM snipProjDictP tree
	
	
snipProjDictP (PProjDict nn t ss)
 = do
	let (TData vCon ts)	= t

	-- See what vars are in the dict
	--	and make a map of new vars.
	--	
 	let dictVs	= nub
			$ catMaybes 
			$ map takeStmtBoundV ss
			
	dictVsNew 	<- mapM (newProjFunVar vCon) dictVs
	let varMap	= Map.fromList $ zip dictVs dictVsNew
	
	-- 
	let (pp, mss')	= unzip $ map (snipProjDictS varMap) ss
	
	return	$ PProjDict nn t (catMaybes mss')
		: pp

snipProjDictP pp
 =	return [pp]


--- newProjFunVar
--	Make 

newProjFunVar :: Var -> Var -> ProjectM Var
newProjFunVar vCon vField
 = do
 	var	<- newVarN NameValue
	return	
		$ var 	{ Var.name = "project_" ++ Var.name vCon ++ "_" ++ Var.name vField 
			, Var.info = [Var.ISourcePos NoSourcePos ]
			, Var.nameModule = Var.nameModule vCon }


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
 -> 	ProjectM (Tree Annot)
 
addProjDictDataTree tree
 = do
	-- Slurp out all the data defs
	let dataDefs	= [p		| p@(PData _ v vs ctors)	<- tree]

 	-- Slurp out all the available projection dictionaries.
	let projMap	= Map.fromList
			$ [(v, p)	| p@(PProjDict _ t@(TData v ts) ss)	<- tree]
		
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
		Nothing	-> [p, PProjDict none (TData v (map varToTWild vs)) []]
		Just _	-> [p]
		
	_		-> [p]

varToTWild v
	= TWild (kindOfSpace $ Var.nameSpace v)
	

-----
-- addProjDictFunsTree
--	Add default field projections to dictionaries for data types.
--
addProjDictFunsTree 
 :: 	Map Var (Top Annot) -> Tree Annot
 -> 	ProjectM (Tree Annot)

addProjDictFunsTree dataMap tree
 	= mapM (addProjDictFunsP dataMap) tree
	
addProjDictFunsP 
	dataMap 
	p@(PProjDict nn projType@(TData v ts) ss)
 = do
	-- Lookup the data def for this type.
 	let (Just (PData _ vData vsData ctors))	
		= Map.lookup v dataMap
	
	let tData	= TData vData (map (\v -> TVar (kindOfSpace $ Var.nameSpace v) v) vsData)
	
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
	let newFieldVs	= dataFieldVs \\ dictVs

	projFunsSS	<- liftM concat
			$  mapM (makeProjFun tData ctors) newFieldVs

	-- Make reference projection functions
	projRFunsSS 	<- liftM concat
			$  mapM (makeProjR_fun tData ctors) dataFieldVs
		
	return 	$ PProjDict nn projType (projFunsSS ++ projRFunsSS ++ ss)
 
addProjDictFunsP dataMap p
 =	return p


makeProjFun 
 :: 	Type
 -> 	[CtorDef Annot] -> Var 
 ->	ProjectM [Stmt Annot]

makeProjFun    tData ctors fieldV
  = do 	
	objV	<- newVarN NameValue
	
	alts	<- liftM catMaybes
  		$  mapM (makeProjFunAlt objV fieldV) ctors

	-- Find the field type for this projection.
	let (resultT:_)	= [dType field 	| CtorDef _ _ fields	<- ctors
					, field			<- fields
					, dLabel field == Just fieldV ]

    	return	[ SSig  none fieldV 	
			(TSig $ (TFun tData resultT pure empty))

		, SBind none (Just fieldV) 
 			(XLambda none objV 
				(XMatch none (Just (XVar none objV)) alts)) ]
				
makeProjFunAlt objV fieldV (CtorDef _ vCon fields)
 = do
	let mFieldIx	= lookup (Just fieldV)
			$ zip (map dLabel fields) [0..]

	bindV		<- newVarN NameValue
			
	return	
	 $ case mFieldIx of
	 	Just ix	-> Just 
			$  AAlt none 
				[GCase none (WConLabel none vCon [(LVar none fieldV, bindV)]) ] 
				(XVar none bindV)

		Nothing	-> Nothing

-----
-- makeProjR

makeProjR_fun
 ::	Type
 ->	[CtorDef Annot] -> Var
 ->	ProjectM [Stmt Annot]
 
makeProjR_fun tData ctors fieldV
 = do	
	funV_		<- newVarN NameValue
	let funV	= funV_ { Var.name = "ref_" ++ Var.name fieldV 
				, Var.nameModule = Var.nameModule fieldV }
		
	objV		<- newVarN NameValue

	alts		<- liftM catMaybes
 			$ mapM (makeProjR_alt objV fieldV) ctors

	-- Find the field type for this projection.
	let (resultT:_)	= [dType field 	| CtorDef _ _ fields	<- ctors
					, field			<- fields
					, dLabel field == Just fieldV ]

	let rData	= case tData of
				TData vData (TVar KRegion rData : _)	
					-> rData
				_ 	-> panic stage
					$ "makeProjR_fun: can't take top region from " 	% tData	% "\n"
					% "  tData = " % show tData			% "\n"

	return	$ 	[ SSig  none funV 	
				(TSig 	$ (TFun tData (TData primTRef [TVar KRegion rData, resultT]) 
						pure
						empty))


			, SBind none (Just funV) 
				(XLambda none objV
					(XMatch none (Just (XVar none objV)) alts))]
				
makeProjR_alt objV fieldV (CtorDef _ vCon fields)
 = do
{- 	let fieldCount	= length $ filter dPrimary fields
	fieldVs		<- replicateM fieldCount (newVarN NameValue)
-}
	let mFieldIx	= lookup (Just fieldV)
			$ zip (map dLabel fields) [0..]
			
	return
	 $ case mFieldIx of
	 	Just ix	-> Just
			$ AAlt none
				[ GCase none (WConLabel none vCon []) ]
				(XApp none 	(XApp none 	(XVar none primProjFieldR) 
						 		(XVar none objV))
						(XConst none (CConstU (LInt ix))))
				
		Nothing	-> Nothing
 
 
-----
-- slurpProjTable
--	Slurp out all the projections in a tree into a projection table
--	for easy lookup
--	
--	BUGS:	If there are multiple tables for a particular type then 
--		later ones replace earlier ones. We should really merge
--		the tables together here.
--
--
type	ProjTable	
	= Map Var [(Type, Map Var Var)]

slurpProjTable
	:: Tree Annot
	-> ProjTable

slurpProjTable tree
 = let
	-- Slurp out the projection dictionaries into maps for later use
	--	in toCore.
	projDictPs
		= [p	| p@(PProjDict _ _ _)	<- tree]
	
	projDictS s
		= case s of
			SBind _ (Just v1) (XVarInst _ v2)	-> Just (v1, v2)
			SBind _ (Just v1) (XVar     _ v2)	-> Just (v1, v2)
			_					-> Nothing

	packProjDict (PProjDict _ t@(TData vCon _) ss)
		= (vCon, (t, Map.fromList $ catMaybes $ map projDictS ss))

	projTable	= Map.gather 
			$ map packProjDict projDictPs
 in 	projTable


