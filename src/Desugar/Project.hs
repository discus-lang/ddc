-- | Desugar projection and type class dictionaries.
--	* For each data type, add a default projection dict if none exists.
--	* Add default projections.
--	* Snip user provided projection functions to top level.
module Desugar.Project
	( projectTree 
	, ProjTable
	, slurpProjTable )
where
import Source.Error
import Desugar.Util
import DDC.Desugar.Exp
import Shared.VarPrim
import Util
import DDC.Desugar.Projections.Base
import DDC.Desugar.Projections.Naming
import DDC.Base.SourcePos
import DDC.Base.DataFormat
import DDC.Base.Literal
import DDC.Main.Error
import DDC.Type
import DDC.Type.Data
import DDC.Var
import DDC.Util.FreeVars
import qualified Data.Set		as Set
import qualified Util.Data.Map		as Map
import qualified Shared.VarUtil		as Var

stage	= "Desugar.Project"


-- Project ----------------------------------------------------------------------------------------
projectTree 
	:: String		-- ^ unique string
	-> ModuleId		-- ^ the name of the current module
	-> Tree Annot 		-- ^ header tree
	-> Tree Annot 		-- ^ source tree
	-> (Tree Annot, [Error])

projectTree unique modName headerTree tree
 = let	(tree', state')
		= runState (projectTreeM modName headerTree tree) 
		$ stateInit unique

   in	(tree', stateErrors state')
		
	
projectTreeM :: ModuleId -> Tree Annot -> Tree Annot -> ProjectM (Tree Annot)
projectTreeM modName headerTree tree
 = do
	-- Slurp out all the data defs
	let dataMap	= Map.fromList
			$ [(dataDefName def, def) 	
				| p@(PData _ def) 
				<- tree ++ headerTree]

	let classDicts	= Map.fromList
			$ [(v, p)	
				| p@(PClassDecl _ v ts vts)
				<- tree ++ headerTree]

	mapM (checkForRedefDataField dataMap) [p | p@(PProjDict{}) <- tree]
	_ <- foldM checkForRedefClassInst Map.empty [p | p@(PClassInst{}) <- tree]

	-- Each data type in the source file should have a projection dictionary
	--	if none exists then make a new empty one.
	treeProjNewDict	<- addProjDictDataTree tree

	-- Add default projections to projection dictionaries.
	treeProjFuns	<- addProjDictFunsTree dataMap treeProjNewDict
	
	-- Snip user functions out of projection dictionaries.
 	treeProjDict	<- snipProjDictTree modName classDicts treeProjFuns

	return treeProjDict


-- | Snip out functions and sigs from projection dictionaries to top level.
--	Also snip class instances while we're here.
snipProjDictTree 
	:: ModuleId 			-- the name of the current module
	-> Map Var (Top SourcePos)	-- class dictionary definitions
	-> Tree SourcePos
	-> ProjectM (Tree SourcePos)

snipProjDictTree modName classDicts tree
 	= liftM concat
 	$ mapM (snipProjDictP modName classDicts) tree
	
-- Snip RHS of bindings in projection dictionaries.
snipProjDictP modName classDicts (PProjDict sp t ss)
 = do
	let (Just (vCon, _, _))	= takeTData t

	-- See what vars are in the dict and make a map of new vars.
 	let dictVs	= Set.toList
			$ Set.unions
			$ map bindingVarsOfStmt ss
			
	dictVsNew 	<- mapM (newProjFunVar sp modName vCon) dictVs
	let varMap	= Map.fromList $ zip dictVs dictVsNew
	
	let (mpp, mss')	= unzip $ map (snipProjDictS varMap) ss
	
	return	$ PProjDict sp t (catMaybes mss')
		: catMaybes mpp


-- Snip RHS of bindings in type class instances.
snipProjDictP modName classDicts 
	pInst@(PClassInst sp vClass ts ssInst)

	-- lookup the class definition for this instance
	| Just pClass	<- Map.lookup vClass classDicts
	= do	(ss', pss)	<- liftM unzip
				$  mapM (snipInstBind modName pClass pInst) ssInst

		return	$ PClassInst sp vClass ts ss'
			: concat pss
	
	| otherwise
	= do	addError $ ErrorUndefinedVar vClass
		return $ [pInst]

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
	-> Top SourcePos		-- ^ the class dict def of this instance
	-> Top SourcePos		-- ^ the class dict instance
	-> Stmt SourcePos		-- ^ the binding in this instance to snip
	-> ProjectM ( Stmt SourcePos
		    , [Top SourcePos])

-- if the RHS is already a var we can leave it as it is.
snipInstBind modName
	pClass pInst 
	bind@(SBind spBind (Just vInst) (XVar{}))
 = 	return (bind, [])

-- otherwise lift it out to top level
snipInstBind modName 
	pDict@(PClassDecl _  vClass  tsClass vtsClass)
	pInst@(PClassInst _  _       tsInst  _)
	sBind@(SBind sp (Just vInst) _)
 = do
	-- create a new top-level variable to use for this binding
 	vTop	<- newInstFunVar sp modName vClass tsInst vInst
	
	-- lookup the type for this instance function and substitute
	--	in the types for this instance
	case lookup vInst vtsClass of
	 Nothing	
	  -> do	addError $ ErrorNotMethodOfClass vInst vClass
		return (sBind, [])
	
	 -- instance function is not defined in the type class declaration
	 Just tInst	
	  -> snipInstBind' modName pDict pInst sBind vTop tInst


-- | Make the type signature for the instance function
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
snipInstBind' modName 
	pDict@(PClassDecl _  vClass  tsClass vtsClass)
	pInst@(PClassInst sp vClass' tsInst  ssInst)
	sBind@(SBind spBind (Just vInst) xx)
	vTop
	tInst
 = do
	let tInst_sub	= subTT_noLoops
				(Map.fromList $ zip tsClass tsInst)
				tInst

	let vsFree	= Set.filter (\v -> not $ Var.isCtorName v) $ freeVars tsInst
	let bks_quant	= map (\v -> (BVar v, let Just k = kindOfSpace $ varNameSpace v in k)) 
			$ Set.toList vsFree
	let tInst_quant	= makeTForall_back bks_quant tInst_sub
	
	-- As we're duplicating information from the original signature
	--	we need to rewrite the binders on FWhere fetters.
	--	It'd probably be nicer to use exists. quantifiers for this instead...
	tInst_fresh	<- freshenCrsEq modName tInst_quant
	
	return	(  SBind spBind (Just vInst) (XVar spBind vTop)
		,  [ PTypeSig spBind [vTop] tInst_fresh
		   , PBind    spBind (Just vTop)  xx])


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
	= ( Just $ PTypeSig  nn vs' t
	  , Nothing )

	| otherwise
	= ( Nothing
	  , Just xx)

-- | Make sure there is a projection dictionary for each data type.
--   If one doesn't exist, then make a new one. 
--   This dictionary will be filled with the default projections by 
--   addProjDictFunTree.
addProjDictDataTree
	:: Tree Annot
	-> ProjectM (Tree Annot)
 
addProjDictDataTree tree
 = do
 	-- Slurp out all the available projection dictionaries.
	let projMap	
		= Map.fromList
		$ catMaybes
		$ map	(\p -> case p of
				PProjDict _ t ss
				 | Just (v, _, ts)	<- takeTData t
				 -> Just (v, p)
					
				_ -> Nothing)
			tree
		
	-- If there is no projection dictionary for a given data type
	--	then make a new empty one. Add new dictionary straight after the data def
	--	to make life easier for the type inference constraint reordering.
	return	$ catMap (addProjDataP projMap) tree
	

addProjDataP projMap p
 = case p of
	PData sp 
	 (DataDef 
		{ dataDefName 	= v 
		, dataDefParams	= vks
		, dataDefCtors	= ctors })
 	 -> case Map.lookup v projMap of
		Nothing	-> [p, PProjDict sp 
					(makeTData v (makeKFuns (map snd vks) kValue) 
								(map varToTBot $ map fst vks))
								[]]
		Just _	-> [p]
		
	_		-> [p]

varToTBot v
	= tBot (let Just k = kindOfSpace $ varNameSpace v in k) 
	

-- | Add default field projections to dictionaries for data types.
--	Abstract data types with no constructors don't have fields,
--	so don't need default field projections.
addProjDictFunsTree 
	:: Map Var DataDef	-- ^ Map of type constructor name to data type def.
	-> Tree Annot		-- ^ The tree to desugar.
	-> ProjectM (Tree Annot)

addProjDictFunsTree dataMap tree
 	= mapM (addProjDictFunsP dataMap) tree
	
addProjDictFunsP 
	dataMap
	p@(PProjDict sp projType ss)
	
 | Just (v, k, ts)	<- takeTData projType
 , Just dataDef		<- Map.lookup v dataMap
 = do	
	let vData	= dataDefName dataDef
	let vksData	= dataDefParams dataDef
	let tsData	= map (\(v, k) -> TVar k $ UVar v) vksData
	let tData	= makeTData vData (makeKFuns (map snd vksData) kValue) tsData
	
	-- See what projections have already been defined.
	let dictVs	= Set.unions
			$ map bindingVarsOfStmt ss
	
	-- Gather the list of all fields in the data def.
	let dataFieldVs	= fieldsOfDataDef dataDef

	-- Only add a projection function if there isn't one
	--	for that field already in the dictionary.
	let newFieldVs	= Set.difference dataFieldVs dictVs

	projFunsSS	<- liftM concat
			$  mapM (makeProjFun sp tData dataDef) 
			$  Set.toList newFieldVs

	-- Make reference projection functions
	projRFunsSS 	<- liftM concat
			$  mapM (makeProjR_fun sp tData dataDef) 
			$  Set.toList dataFieldVs
		
	return 	$ PProjDict sp projType (projFunsSS ++ projRFunsSS ++ ss)
 
addProjDictFunsP dataMap p
 =	return p


makeProjFun 
	:: SourcePos 
	-> Type
	-> DataDef
	-> Var 
	-> ProjectM [Stmt Annot]

makeProjFun sp tData dataDef fieldV
  = do 	objV	<- newVarN NameValue
	
	alts	<- liftM catMaybes
  		$  mapM (makeProjFunAlt sp objV fieldV) 
		$  Map.elems $ dataDefCtors dataDef

	-- Find the field type for this projection.
	let Just resultT
		= liftM stripToBodyT 
		$ lookupTypeOfFieldFromDataDef fieldV dataDef

    	return	[ SSig  sp [fieldV]
			(makeTFun tData resultT tPure tEmpty) 

		, SBind sp (Just fieldV) 
 			(XLambda sp objV 
				(XMatch sp (Just (XVar sp objV)) alts)) ]

		
makeProjFunAlt sp vObj vField ctorDef
 = do	let mFieldIx	
		= liftM stripToBodyT
		$ lookupTypeOfNamedFieldFromCtorDef vField ctorDef

	vBind	<- newVarN NameValue
			
	case mFieldIx of
	 Just ix	
	  -> return $ Just 
	  $  AAlt sp	[GCase sp (WConLabel sp (ctorDefName ctorDef) 
				  [(LVar sp vField, vBind)]) ] 
			(XVar sp vBind)

	 Nothing 
	  -> return Nothing

makeProjR_fun
	:: SourcePos
	-> Type
	-> DataDef
	-> Var
	-> ProjectM [Stmt Annot]
 
makeProjR_fun sp tData dataDef vField
 = do	funV_	<- newVarN NameValue
	let funV = funV_ 
		{ varName 	= "ref_" ++ varName vField
		, varModuleId 	= varModuleId vField }
		
	vObj	<- newVarN NameValue
	alts	<- liftM catMaybes
 		$  mapM (makeProjR_alt sp vObj vField) 
		$  Map.elems 
		$  dataDefCtors dataDef

	-- Find the field type for this projection.
	let Just resultT
		= liftM stripToBodyT
		$ lookupTypeOfFieldFromDataDef vField dataDef 

	let rData	
		= case tData of
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
				(XLambda sp vObj
					(XMatch sp (Just (XVar sp vObj)) alts))]

				
makeProjR_alt sp objV fieldV ctor
 = do	let vCon	= ctorDefName ctor
	
	let (mFieldIx :: Maybe Int)
		= Map.lookup fieldV $ ctorDefFields ctor
			
	return
	 $ case mFieldIx of
	 	Just ix	-> Just
			$ AAlt sp
				[ GCase sp (WConLabel sp vCon []) ]
				(XApp sp 	(XApp sp 	(XVar sp primProjFieldR) 
						 		(XVar sp objV))
						(XLit sp (LiteralFmt (LInt $ fromIntegral ix) (UnboxedBits 32))))
				
		Nothing	-> Nothing
 
 
-- | Slurp out all the projections in a tree into a projection table
--   for easy lookup
--	
--   BUGS: If there are multiple tables for a particular type then 
--	   later ones replace earlier ones. We should really merge
--	   the tables together here.
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


-- | Check for projection names that collide with field names of the Data
--   definition they are projecting over. Log any that are found as errors.
checkForRedefDataField :: Map Var DataDef -> Top Annot -> ProjectM ()
checkForRedefDataField dataMap (PProjDict _ tt ss)
 | Just (pvar, _, _)	<- takeTData tt
 = case Map.lookup pvar dataMap of
 	Nothing 	-> return ()
	Just dname	-> mapM_ (checkSBindFunc dname pvar (fieldNames dname)) ss

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


checkSBindFunc :: DataDef -> Var -> Map String Var -> Stmt Annot -> ProjectM ()
checkSBindFunc def pvar dfmap (SBind _ (Just v) _)
 = case Map.lookup (varName v) dfmap of
	Nothing		-> return ()
	Just redef	
		-> addError 
		$ ErrorProjectRedefDataField redef pvar 
		$ dataDefName def

checkSBindFunc _ _ _ _ = return ()


fieldNames :: DataDef -> Map String Var
fieldNames dataDef
	= Map.fromList
	$ [ (varName d, d)
		| d <- Set.toList $ fieldsOfDataDef dataDef ]

