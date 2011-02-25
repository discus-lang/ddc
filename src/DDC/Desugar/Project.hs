-- | Desugar projection and type class dictionaries.
--   For each data type, add a default projection dict if none exists.
--   Add default projections.
--   Snip user provided projection functions to top level.
module DDC.Desugar.Project
	( projectTree 
	, ProjTable
	, slurpProjTable )
where
import Shared.VarPrim
import Util
import DDC.Source.Error
import DDC.Desugar.Exp
import DDC.Desugar.Bits
import DDC.Desugar.Projections.Base
import DDC.Desugar.Projections.Snip
import DDC.Desugar.Projections.Check
import DDC.Base.SourcePos
import DDC.Base.DataFormat
import DDC.Base.Literal
import DDC.Main.Error
import DDC.Type
import DDC.Type.Data
import DDC.Type.SigMode
import DDC.Var
import qualified Data.Set		as Set
import qualified Data.MapUtil		as Map

stage	= "DDC.Desugar.Project"

-- Project ----------------------------------------------------------------------------------------
projectTree 
	:: String	-- ^ unique string
	-> ModuleId	-- ^ the name of the current module
	-> Tree Annot 	-- ^ header tree
	-> Tree Annot 	-- ^ source tree
	-> (Tree Annot, [Error])

projectTree unique modName headerTree tree
 = let	(tree', state')
		= runState (projectTreeM modName headerTree tree) 
		$ stateInit unique

   in	(tree', stateErrors state')


-- | Rewrite projection dictionaries in this tree.
projectTreeM 
	:: ModuleId	-- ^ id of current module, used to name new top-level bindings.
	-> Tree Annot
	-> Tree Annot
	-> ProjectM (Tree Annot)
projectTreeM modName headerTree tree
 = do
	-- | Slurp out all the data defs
	let dataMap	= Map.fromList
			$ [(dataDefName def, def) 	
				| p@(PData _ def) 
				<- tree ++ headerTree]

	-- | Slurp out the class dictionaries.
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

    	return	[ SSig  sp SigModeMatch [fieldV]
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

	return	$ 	[ SSig  sp SigModeMatch [funV]
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

	projTable	= Map.collate 
			$ map packProjDict projDictPs
 in 	projTable


