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

import Source.Error

import Desugar.Util
import Desugar.Bits
import Desugar.Exp

import Type.Exp
import Type.Util
import Type.Plate.FreeVars

import Shared.Pretty
import Shared.Exp
import Shared.Base
import Shared.Literal
import Shared.VarPrim
import Shared.Error

import qualified Shared.Var	as Var
import qualified Shared.VarBind	as Var
import qualified Shared.VarUtil	as Var
import Shared.Var		(Var, NameSpace(..), Module)

import Util
import qualified Util.Map	as Map

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

-----
stage	= "Desugar.Project"

-- State ------------------------------------------------------------------------------------------
data ProjectS
	= ProjectS
	{ stateVarGen	:: Var.VarBind
	, stateErrors	:: [Error] }
	
stateInit unique
	= ProjectS
	{ stateVarGen	= Var.XBind unique 0
	, stateErrors	= [] }
	
newVarN :: NameSpace -> ProjectM Var
newVarN	space
 = do
 	varBind		<- gets stateVarGen
	let varBind'	= Var.incVarBind varBind
	modify $ \s -> s { stateVarGen = varBind' }

	let var		= (Var.new $ pprStrPlain varBind)
			{ Var.bind	= varBind
			, Var.nameSpace	= space }
	return var

addError :: Error -> ProjectM ()
addError err
	= modify $ \s -> s { stateErrors = err : stateErrors s }

type	ProjectM	= State ProjectS
type	Annot		= SourcePos

-- Project ----------------------------------------------------------------------------------------
projectTree 
	:: String		-- unique string
	-> Module		-- the name of the current module
	-> Tree Annot 		-- header tree
	-> Tree Annot 		-- source tree
	-> (Tree Annot, [Error])

projectTree unique moduleName headerTree tree
 = let	(tree', state')
		= runState (projectTreeM moduleName headerTree tree) 
		$ stateInit unique
   in	(tree', stateErrors state')
		
	
projectTreeM :: Module -> Tree Annot -> Tree Annot -> ProjectM (Tree Annot)
projectTreeM moduleName headerTree tree
 = do
	-- Slurp out all the data defs
	let dataMap	= Map.fromList
			$ [(v, p) 	| p@(PData _ v vs ctors) 
					<- tree ++ headerTree]

	let classDicts	= Map.fromList
			$ [(v, p)	| p@(PClassDict _ v ts cs vts)
					<- tree ++ headerTree]

	-- Each data type in the source file should have a projection dictionary
	--	if none exists then make a new empty one.
	treeProjNewDict	<- addProjDictDataTree tree

	-- Add default projections to projection dictionaries.
	treeProjFuns	<- addProjDictFunsTree dataMap treeProjNewDict
	
	-- Snip user functions out of projection dictionaries.
 	treeProjDict	<- snipProjDictTree moduleName classDicts treeProjFuns

	return treeProjDict


-- | Snip out functions and sigs from projection dictionaries to top level.
--	Also snip class instances while we're here.

snipProjDictTree 
	:: Module 			-- the name of the current module
	-> Map Var (Top SourcePos)	-- class dictionary definitions
	-> Tree SourcePos
	-> ProjectM (Tree SourcePos)

snipProjDictTree moduleName classDicts tree
 	= liftM concat
 	$ mapM (snipProjDictP moduleName classDicts) tree
	
-- Snip RHS of bindings in projection dictionaries.
snipProjDictP moduleName classDicts (PProjDict sp t ss)
 = do
	let (TData vCon _)	= t

	-- See what vars are in the dict and make a map of new vars.
 	let dictVs	= nub
			$ catMaybes 
			$ map takeStmtBoundV ss
			
	dictVsNew 	<- mapM (newProjFunVar sp moduleName vCon) dictVs
	let varMap	= Map.fromList $ zip dictVs dictVsNew
	
	-- 
	let (mpp, mss')	= unzip $ map (snipProjDictS varMap) ss
	
	return	$ PProjDict sp t (catMaybes mss')
		: catMaybes mpp


-- Snip RHS of bindings in type class instances.
snipProjDictP moduleName classDicts 
	pInst@(PClassInst sp vClass ts context ssInst)

	-- lookup the class definition for this instance
	| Just pClass	<- Map.lookup vClass classDicts
	= do	
		(ss', pss)	<- liftM unzip
				$  mapM (snipInstBind moduleName pClass pInst) ssInst

		return	$ PClassInst sp vClass ts context (ss')
			: concat pss
	
	| otherwise
	= panic stage $ "snipProjDictP: Type class '" % vClass % "' is not in scope."


-- Snip field initializers
snipProjDictP moduleName classDicts (PData nn vData vsArg ctorDefs)
 = do	(ctorDefs', psNew)	
 		<- liftM unzip
		$ mapM (snipCtorDef moduleName nn vData) ctorDefs
		
	return	$ PData nn vData vsArg ctorDefs'
		: concat psNew

snipProjDictP _ _ pp
 =	return [pp]



-- snipInstBind ------------------------------------------------------------------------------------
-- snip RHS of bindings in type class instances
snipInstBind
	:: Module
	-> Top SourcePos		-- the class dict def of this instance
	-> Top SourcePos		-- the class dict instance
	-> Stmt SourcePos		-- the binding in this instance to snip
	-> ProjectM ( Stmt SourcePos
		    , [Top SourcePos])

-- if the RHS is already a var we can leave it as it is.
snipInstBind moduleName
	pClass pInst 
	bind@(SBind spBind (Just vInst) (XVar{}))
 = 	return (bind, [])

-- otherwise lift it out to top level
snipInstBind moduleName 
	pDict@(PClassDict _  vClass  tsClass _       vtsClass)
	pInst@(PClassInst _  _       tsInst  _       _)
	sBind@(SBind sp (Just vInst) _)
 = do
	-- create a new top-level variable to use for this binding
 	vTop	<- newInstFunVar sp moduleName vClass tsInst vInst
	
	-- lookup the type for this instance function and substitute
	--	in the types for this instance
	case lookup vInst vtsClass of
	 Nothing	
	  -> do	addError $ ErrorNotMethodOfClass vInst vClass
		return (sBind, [])
		
	 Just tInst	-> snipInstBind' moduleName pDict pInst sBind vTop tInst

snipInstBind' moduleName 
	pDict@(PClassDict _  vClass  tsClass ccClass vtsClass)
	pInst@(PClassInst sp vClass' tsInst  ccInst  ssInst)
	sBind@(SBind spBind (Just vInst) xx)
	vTop
	tInst
 = do
	-- we also need to quantify over any free variables in the parameters
	-- eg for:
	--	instance Int %r1 where
	--	 (+) = ...
	--
	-- sig for (+) is 
	--	(+) :: forall %r1 . ...
	--
	let tInst_sub	= subTT_noLoops
				(Map.fromList $ zip tsClass tsInst)
				tInst

	let vsFree	= Set.filter (\v -> not $ Var.isCtorName v) $ freeVars tsInst
	let vks_quant	= map (\v -> (v, kindOfSpace $ Var.nameSpace v)) $ Set.toList vsFree
	let tInst_quant	= makeTForall vks_quant tInst_sub
	
	return	(  SBind spBind (Just vInst) (XVar spBind vTop)
		,  [ PSig spBind vTop tInst_quant
		   , PBind spBind (Just vTop)  xx])


-- snip expressions out of data field intialisers in this ctor def
snipCtorDef 
	:: Module		-- the current module
	-> a			-- annot to use on new code
	-> Var 			-- var of data type
	-> CtorDef a		-- ctor def to transform
	-> ProjectM
		( CtorDef a	-- new ctor def
		, [Top a])	-- new top level bindings

snipCtorDef moduleName sp vData (CtorDef nn vCtor dataFields)
 = do	(dataFields', psNew)	
 		<- liftM unzip 
		$ mapM (snipDataField moduleName sp vData vCtor) dataFields
		
	return	( CtorDef nn vCtor dataFields'
		, concat psNew)
 
 
-- snip expresisons out of data field initialisers
snipDataField 
	:: Module		-- the current module
	-> a			-- annot to use on new code
	-> Var			-- var of data type
	-> Var			-- var of contructor
	-> DataField (Exp a) Type 
	-> ProjectM 
		( DataField (Exp a) Type	-- snipped data field
		, [Top a])			-- new top level bindings

snipDataField moduleName sp vData vCtor field
	-- no initialiser
	| Nothing	<- dInit field
	= return 
		( field
		, [])

	-- leave vars there
 	| Just XVar{}	<- dInit field
	= return 
		( field
		, [])
	
	-- snip other expressions
	| Just xInit	<- dInit  field
	, Just vField	<- dLabel field
	= do	var_	<- newVarN NameValue
		let var	= var_
			{ Var.name = "init_" 
				++ Var.name vData  ++ "_" 
				++ Var.name vCtor  ++ "_" 
				++ Var.name vField
			, Var.nameModule	= moduleName }

		varL	<- newVarN NameValue
		varR	<- newVarN NameRegion
				
		return	( field { dInit = Just $ XVar sp var }
			, [ PSig  sp var (TFun 	(TData primUnit [TVar KRegion varR]) 
						(dType field) 
						(TBot KEffect) (TBot KClosure))
			  , PBind sp (Just var) (XLambda sp varL xInit)])


-- | Create a name for a top level projection function.
--	Add the type and projection names to the var to make the CoreIR readable.
newProjFunVar :: SourcePos -> Module -> Var -> Var -> ProjectM Var
newProjFunVar 
	src
	moduleName@(Var.ModuleAbsolute ms)
	vCon vField
 = do
 	var	<- newVarN NameValue
	return	
	 $ var 	{ Var.name 
	 		= Var.deSymString
			$ "project_" 
	 		++ Var.name vCon 	++ "_" 
			++ Var.name vField 
			
		, Var.info = [Var.ISourcePos src ]
		, Var.nameModule = moduleName }


-- | Create a name for a top level type class instance function
--	Add the type class and function names to the var to make the CoreIR readable.
newInstFunVar :: SourcePos -> Module -> Var -> [Type] -> Var -> ProjectM Var
newInstFunVar 
	src
	moduleName@(Var.ModuleAbsolute ms)
	vClass 
	tsArgs
	vInst
 = do
 	var	<- newVarN NameValue
	return	
	 $ var 	{ Var.name 
	 		= Var.deSymString
			$ "instance_" 
			++ Var.name vClass	 	++ "_" 
			++ catMap makeTypeName tsArgs	++ "_"
			++ Var.name vInst

		, Var.info = [Var.ISourcePos src ]

		, Var.nameModule = moduleName }

-- | Make a printable name from a type
--	TODO: do this more intelligently, in a way guaranteed not to clash with other types
makeTypeName :: Type -> String
makeTypeName tt
 = case tt of
 	TFun t1 t2 eff clo	-> "Fun" ++ makeTypeName t1 ++ makeTypeName t2
	TData v ts		-> (Var.name v) ++ (catMap makeTypeName ts)
	TVar k v		-> ""
	TWild k			-> ""


-- | Snip the RHS of this statement down to a var
snipProjDictS 
	:: Map Var Var 
	-> Stmt a 
	-> ( Maybe (Top a)
	   , Maybe (Stmt a))

snipProjDictS varMap xx
	| SBind nn (Just v) x	<- xx
	, Just v'		<- Map.lookup v varMap
	= ( Just $ PBind nn (Just v') x
	  , Just $ SBind nn (Just v)  (XVar nn v'))
	  	
	| SSig  nn v  t		<- xx
	, Just v'		<- Map.lookup v varMap
	= ( Just $ PSig  nn v'  t
	  , Nothing )

	| otherwise
	= ( Nothing
	  , Just xx)

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
--	let dataDefs	= [p		| p@(PData _ v vs ctors)	<- tree]

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
	PData sp v vs ctors
 	 -> case Map.lookup v projMap of
		Nothing	-> [p, PProjDict sp (TData v (map varToTWild vs)) []]
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
	p@(PProjDict sp projType@(TData v ts) ss)
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
			$  mapM (makeProjFun sp tData ctors) newFieldVs

	-- Make reference projection functions
	projRFunsSS 	<- liftM concat
			$  mapM (makeProjR_fun sp tData ctors) dataFieldVs
		
	return 	$ PProjDict sp projType (projFunsSS ++ projRFunsSS ++ ss)
 
addProjDictFunsP dataMap p
 =	return p


makeProjFun 
 :: 	SourcePos 
 -> 	Type
 -> 	[CtorDef Annot] -> Var 
 ->	ProjectM [Stmt Annot]

makeProjFun sp tData ctors fieldV
  = do 	
	objV	<- newVarN NameValue
	
	alts	<- liftM catMaybes
  		$  mapM (makeProjFunAlt sp objV fieldV) ctors

	-- Find the field type for this projection.
	let (resultT:_)	= [dType field 	| CtorDef _ _ fields	<- ctors
					, field			<- fields
					, dLabel field == Just fieldV ]

    	return	[ SSig  sp fieldV 	
			(TFun tData resultT pure empty) 

		, SBind sp (Just fieldV) 
 			(XLambda sp objV 
				(XMatch sp (Just (XVar sp objV)) alts)) ]
				
makeProjFunAlt sp objV fieldV (CtorDef _ vCon fields)
 = do
	let (mFieldIx :: Maybe Int)
		= lookup (Just fieldV)
		$ zip (map dLabel fields) [0..]

	bindV		<- newVarN NameValue
			
	return	
	 $ case mFieldIx of
	 	Just ix	-> Just 
			$  AAlt sp 
				[GCase sp (WConLabel sp vCon [(LVar sp fieldV, bindV)]) ] 
				(XVar sp bindV)

		Nothing	-> Nothing

-----
-- makeProjR

makeProjR_fun
 ::	SourcePos
 ->	Type
 ->	[CtorDef Annot] -> Var
 ->	ProjectM [Stmt Annot]
 
makeProjR_fun sp tData ctors fieldV
 = do	
	funV_		<- newVarN NameValue
	let funV	= funV_ { Var.name = "ref_" ++ Var.name fieldV 
				, Var.nameModule = Var.nameModule fieldV }
		
	objV		<- newVarN NameValue

	alts		<- liftM catMaybes
 			$ mapM (makeProjR_alt sp objV fieldV) ctors

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

	return	$ 	[ SSig  sp funV 	
				(TFun tData (TData primTRef [TVar KRegion rData, resultT]) 
						pure
						empty)


			, SBind sp (Just funV) 
				(XLambda sp objV
					(XMatch sp (Just (XVar sp objV)) alts))]
				
makeProjR_alt sp objV fieldV (CtorDef _ vCon fields)
 = do
	let (mFieldIx :: Maybe Int)
		= lookup (Just fieldV)
		$ zip (map dLabel fields) [0..]
			
	return
	 $ case mFieldIx of
	 	Just ix	-> Just
			$ AAlt sp
				[ GCase sp (WConLabel sp vCon []) ]
				(XApp sp 	(XApp sp 	(XVar sp primProjFieldR) 
						 		(XVar sp objV))
						(XConst sp (CConstU (LInt ix))))
				
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


