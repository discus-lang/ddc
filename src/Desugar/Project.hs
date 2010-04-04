-- | Desugar projection and type class dictionaries.

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
import Desugar.Exp
import Type.Exp
import Type.Util
import Type.Plate.FreeVars
import Shared.Exp
import Shared.VarPrim
import Util
import DDC.Base.SourcePos
import DDC.Base.DataFormat
import DDC.Base.Literal
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Var
import qualified Data.Set		as Set
import qualified Util.Data.Map		as Map
import qualified Shared.VarUtil		as Var

-----
stage	= "Desugar.Project"

-- State ------------------------------------------------------------------------------------------
data ProjectS
	= ProjectS
	{ stateVarGen	:: VarId
	, stateErrors	:: [Error] }
	
stateInit unique
	= ProjectS
	{ stateVarGen	= VarId unique 0
	, stateErrors	= [] }
	
newVarN :: NameSpace -> ProjectM Var
newVarN	space
 = do
 	varBind		<- gets stateVarGen
	let varBind'	= incVarId varBind
	modify $ \s -> s { stateVarGen = varBind' }

	let var		= (varWithName $ (charPrefixOfSpace space : pprStrPlain varBind))
			{ varId	= varBind
			, varNameSpace	= space }
	return var

addError :: Error -> ProjectM ()
addError err
	= modify $ \s -> s { stateErrors = err : stateErrors s }

type	ProjectM	= State ProjectS
type	Annot		= SourcePos

-- Project ----------------------------------------------------------------------------------------
projectTree 
	:: String		-- unique string
	-> ModuleId		-- the name of the current module
	-> Tree Annot 		-- header tree
	-> Tree Annot 		-- source tree
	-> (Tree Annot, [Error])

projectTree unique moduleName headerTree tree
 = let	(tree', state')
		= runState (projectTreeM moduleName headerTree tree) 
		$ stateInit unique
   in	(tree', stateErrors state')
		
	
projectTreeM :: ModuleId -> Tree Annot -> Tree Annot -> ProjectM (Tree Annot)
projectTreeM moduleName headerTree tree
 = do
	-- Slurp out all the data defs
	let dataMap	= Map.fromList
			$ [(v, p) 	| p@(PData _ v vs ctors) 
					<- tree ++ headerTree]

	let classDicts	= Map.fromList
			$ [(v, p)	| p@(PClassDict _ v ts vts)
					<- tree ++ headerTree]

	mapM (checkForRedefDataField dataMap) [p | p@(PProjDict{}) <- tree]
	_ <- foldM checkForRedefClassInst Map.empty [p | p@(PClassInst{}) <- tree]

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
	:: ModuleId 			-- the name of the current module
	-> Map Var (Top SourcePos)	-- class dictionary definitions
	-> Tree SourcePos
	-> ProjectM (Tree SourcePos)

snipProjDictTree moduleName classDicts tree
 	= liftM concat
 	$ mapM (snipProjDictP moduleName classDicts) tree
	
-- Snip RHS of bindings in projection dictionaries.
snipProjDictP moduleName classDicts (PProjDict sp t ss)
 = do
	let (Just (vCon, _, _))	= takeTData t

	-- See what vars are in the dict and make a map of new vars.
 	let dictVs	= Set.toList
			$ Set.unions
			$ map bindingVarsOfStmt ss
			
	dictVsNew 	<- mapM (newProjFunVar sp moduleName vCon) dictVs
	let varMap	= Map.fromList $ zip dictVs dictVsNew
	
	-- 
	let (mpp, mss')	= unzip $ map (snipProjDictS varMap) ss
	
	return	$ PProjDict sp t (catMaybes mss')
		: catMaybes mpp


-- Snip RHS of bindings in type class instances.
snipProjDictP moduleName classDicts 
	pInst@(PClassInst sp vClass ts ssInst)

	-- lookup the class definition for this instance
	| Just pClass	<- Map.lookup vClass classDicts
	= do	(ss', pss)	<- liftM unzip
				$  mapM (snipInstBind moduleName pClass pInst) ssInst

		return	$ PClassInst sp vClass ts ss'
			: concat pss
	
	| otherwise
	= do	addError $ ErrorUndefinedVar vClass
		return $ [pInst]


-- Snip field initializers
snipProjDictP moduleName classDicts (PData nn vData vsArg ctorDefs)
 = do	(ctorDefs', psNew)	
 		<- liftM unzip
		$ mapM (snipCtorDef moduleName nn vData) ctorDefs
		
	return	$ PData nn vData vsArg ctorDefs'
		: concat psNew

snipProjDictP _ _ pp
 =	return [pp]



-- | For each binding in a type class instance, rename the binding and shift it to top level.
--   eg:
--	class Show a where
--	 show :: TYPE
--
--	instance Show Bool where
--	 show = EXP
--
--   yields:
--	class Show a where
--	 show :: TYPE
--
--	instance Show Bool where
--	 show = instance_Show_Bool
--	
--	instance_Show_Bool :: forall a. TYPE
--	instance_Show_Bool =  EXP
--		
snipInstBind
	:: ModuleId
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
	pDict@(PClassDict _  vClass  tsClass vtsClass)
	pInst@(PClassInst _  _       tsInst  _)
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
	
	 -- instance function is not defined in the type class declaration
	 Just tInst	
	  -> snipInstBind' moduleName pDict pInst sBind vTop tInst


-- Make the type signature for the instance function
--	we also need to quantify over any free variables in the class
--	arguments
--
-- eg for:
--	instance Int %r1 where
--	  (+) = ...
--
-- the type signature for for (+) is 
--	(+) :: forall %r1 . ...
--
snipInstBind' moduleName 
	pDict@(PClassDict _  vClass  tsClass vtsClass)
	pInst@(PClassInst sp vClass' tsInst  ssInst)
	sBind@(SBind spBind (Just vInst) xx)
	vTop
	tInst
 = do
	let tInst_sub	= subTT_noLoops
				(Map.fromList $ zip tsClass tsInst)
				tInst

	let vsFree	= Set.filter (\v -> not $ Var.isCtorName v) $ freeVars tsInst
	let vks_quant	= map (\v -> (v, kindOfSpace $ varNameSpace v)) $ Set.toList vsFree
	let tInst_quant	= makeTForall_back vks_quant tInst_sub
	
	-- As we're duplicating information from the original signature
	--	we need to rewrite the binders on FWhere fetters.
	--	It'd probably be nicer to use exists. quantifiers for this instead...
	tInst_fresh	<- freshenCrsEq moduleName tInst_quant
	
	return	(  SBind spBind (Just vInst) (XVar spBind vTop)
		,  [ PSig  spBind [vTop] tInst_fresh
		   , PBind spBind (Just vTop)  xx])

-- 
freshenCrsEq :: ModuleId -> Type -> ProjectM Type
freshenCrsEq mid tt
 = case tt of
	TForall b k t	
	 -> do	t'	<- freshenCrsEq mid t
		return	$ TForall b k t'
	
	TFetters t fs
	 -> do	let takeSub	ff
		     = case ff of
			FWhere (TVar k v) _
			 -> do	vFresh	<- freshenV mid v
				return	$  Just (TVar k v, TVar k vFresh)
				
			_ -> return Nothing
			
		vsSub	<- liftM (Map.fromList . catMaybes)
			$ mapM takeSub fs
			
		return	$ subTT_all vsSub tt
		
	_ -> return tt

freshenV :: ModuleId -> Var -> ProjectM Var		
freshenV mod v
 = do	vNew		<- newVarN (varNameSpace v)
	let vFresh	
		= v 	{ varId		= varId vNew 
			, varModuleId	= mod }
			
	return vFresh



-- snip expressions out of data field intialisers in this ctor def
snipCtorDef 
	:: ModuleId		-- the current module
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
	:: ModuleId		-- the current module
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
 	| Just (XVar sp v)	<- dInit field
	, not $ Var.isCtorName v
	= return 
		( field
		, [])
	
	-- snip other expressions
	| Just xInit	<- dInit  field
	, Just vField	<- dLabel field
	= do	var_	<- newVarN NameValue
		let var	= var_
			{ varName = "init_" 
				++ varName vData  ++ "_" 
				++ varName vCtor  ++ "_" 
				++ varName vField
			, varModuleId	= moduleName }

		varL	<- newVarN NameValue
		varR	<- newVarN NameRegion
				
		return	( field { dInit = Just $ XVar sp var }
			, [ PSig  sp [var] (makeTFun (makeTData primTUnit kValue []) 
						(dType field) 
						tPure tEmpty)
			  , PBind sp (Just var) (XLambda sp varL xInit)])


-- | Create a name for a top level projection function.
--	Add the type and projection names to the var to make the CoreIR readable.
newProjFunVar :: SourcePos -> ModuleId -> Var -> Var -> ProjectM Var
newProjFunVar 
	src
	moduleName@(ModuleId ms)
	vCon vField
 = do
 	var	<- newVarN NameValue
	return	
	 $ var 	{ varName 
	 		= Var.deSymString
			$ "project_" 
	 		++ varName vCon 	++ "_" 
			++ varName vField 
			
		, varInfo 	= [ISourcePos src ]
		, varModuleId	= moduleName }


-- | Create a name for a top level type class instance function
--	Add the type class and function names to the var to make the CoreIR readable.
newInstFunVar :: SourcePos -> ModuleId -> Var -> [Type] -> Var -> ProjectM Var
newInstFunVar 
	src
	moduleName@(ModuleId ms)
	vClass 
	tsArgs
	vInst
 = do
 	var	<- newVarN NameValue
	return	
	 $ var 	{ varName 
	 		= Var.deSymString
			$ "instance_" 
			++ varName vClass	 	++ "_" 
			++ catMap makeTypeName tsArgs	++ "_"
			++ varName vInst

		, varInfo 	= [ISourcePos src ]
		, varModuleId 	= moduleName }

-- | Make a printable name from a type
--	TODO: do this more intelligently, in a way guaranteed not to clash with other types
makeTypeName :: Type -> String
makeTypeName tt
 = case tt of
	TApp{}
	 | Just (t1, t2, eff, clo)	<- takeTFun tt
	 -> "Fun" ++ makeTypeName t1 ++ makeTypeName t2
	
	 | Just (v, _, ts)		<- takeTData tt
	 -> varName v ++ catMap makeTypeName ts

	TCon{}
	 | Just (v, _, ts)		<- takeTData tt
	 -> varName v ++ catMap makeTypeName ts

	TVar k v		-> ""


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
	  	
	| SSig  nn vs t		<- xx
	, Just vs'		<- sequence $ map (\v -> Map.lookup v varMap) vs
	= ( Just $ PSig  nn vs' t
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
			$ catMaybes
			$ map
				(\p -> case p of
					PProjDict _ t ss
					 | Just (v, _, ts)	<- takeTData t
					 -> Just (v, p)
					
					_ -> Nothing)
				tree
		
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
		Nothing	-> [p, PProjDict sp (makeTData v  (makeDataKind vs) (map varToTBot vs)) []]
		Just _	-> [p]
		
	_		-> [p]

varToTBot v
	= TBot (kindOfSpace $ varNameSpace v)
	

-- | Add default field projections to dictionaries for data types.
--	Abstract data types with no constructors don't have fields,
--	so don't need default field projections.
addProjDictFunsTree 
 :: 	Map Var (Top Annot) -> Tree Annot
 -> 	ProjectM (Tree Annot)

addProjDictFunsTree dataMap tree
 	= mapM (addProjDictFunsP dataMap) tree
	
addProjDictFunsP 
	dataMap 
	p@(PProjDict sp projType ss)
 | Just (v, k, ts)			<- takeTData projType
 , Just (PData _ vData vsData ctors)	<- Map.lookup v dataMap
 = do	
	let tsData	= map (\v -> TVar (kindOfSpace $ varNameSpace v) v) vsData
	let tData	= makeTData vData (makeDataKind vsData) tsData
	
	-- See what projections have already been defined.
	let dictVs	= Set.toList
			$ Set.unions
			$ map bindingVarsOfStmt ss
	
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

    	return	[ SSig  sp [fieldV]
			(makeTFun tData resultT tPure tEmpty) 

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
	let funV	= funV_ { varName 	= "ref_" ++ varName fieldV 
				, varModuleId 	= varModuleId fieldV }
		
	objV		<- newVarN NameValue

	alts		<- liftM catMaybes
 			$ mapM (makeProjR_alt sp objV fieldV) ctors

	-- Find the field type for this projection.
	let (resultT:_)	= [dType field 	| CtorDef _ _ fields	<- ctors
					, field			<- fields
					, dLabel field == Just fieldV ]

	let rData	= case tData of
				TApp{}
				 | Just (vData, _, (TVar kR rData : _))	<- takeTData tData
				 , kR == kRegion 
				 -> rData

				_ 	-> panic stage
					$ "makeProjR_fun: can't take top region from " 	% tData	% "\n"
					% "  tData = " % show tData			% "\n"

	return	$ 	[ SSig  sp [funV]
				(makeTFun tData (makeTData 
							primTRef 
							(KFun kRegion (KFun kValue kValue))
							[TVar kRegion rData, resultT]) 
						tPure
						tEmpty)


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
						(XLit sp (LiteralFmt (LInt $ fromIntegral ix) (UnboxedBits 32))))
				
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

	packProjDict (PProjDict _ t ss)
	 = case takeTData t of
		Just (vCon, _, _)
		 -> (vCon, (t, Map.fromList $ catMaybes $ map projDictS ss))

	projTable	= Map.gather 
			$ map packProjDict projDictPs
 in 	projTable


----------------------------------------------------------------------------------------------------
-- Check for projection names that collide with field names of the Data
-- definition they are projecting over. Log any that are found as errors.
--
checkForRedefDataField :: Map Var (Top Annot) -> Top Annot -> ProjectM ()
checkForRedefDataField dataMap (PProjDict _ tt ss)
 | Just (pvar, _, _)	<- takeTData tt
 = case Map.lookup pvar dataMap of
 	Nothing -> return ()
	Just dname -> mapM_ (checkSBindFunc dname pvar (fieldNames dname)) ss

checkForRedefDataField _ _ = return ()


checkForRedefClassInst :: Map String Var -> Top Annot -> ProjectM (Map String Var)
checkForRedefClassInst map ci@(PClassInst sp v tl@(TApp (TCon tc) _ : _) _)
 = do	let key = varName v ++ " " ++ varName (tyConName tc)
	case Map.lookup key map of
          Nothing	-> return $ Map.insert key (tyConName tc) map
	  Just redef	->
          	do	addError $ ErrorRedefClassInst v redef (tyConName tc)
                	return map

checkForRedefClassInst map _  = return map


checkSBindFunc :: Top Annot -> Var -> Map String Var -> Stmt Annot -> ProjectM ()
checkSBindFunc (PData _ dvar _ _) pvar dfmap (SBind _ (Just v) _)
 = case Map.lookup (varName v) dfmap of
	Nothing		-> return ()
	Just redef	-> addError $ ErrorProjectRedefDataField redef pvar dvar

checkSBindFunc _ _ _ _ = return ()


fieldNames :: Top Annot -> Map String Var
fieldNames (PData _ _ _ ctors)
 = Map.fromList $ map (\ d -> (varName d, d))
		$ catMaybes
		$ map dLabel
		$ concat
 		$ map (\(CtorDef _ _ dfields) -> dfields) ctors


