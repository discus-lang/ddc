-- | Slurp out type constraints from the desugared IR.

module Desugar.Slurp.Slurp
	( slurpTreeM )
where

-----
import Data.Map (Map)
import Util

import qualified	Debug.Trace	as Debug

-----
import Shared.Error
import qualified Shared.Error	as Error

import qualified Shared.Var	as Var

import qualified Data.Map	as Map
import Data.Map			(Map)

import Shared.Exp

import Type.Exp
import Type.Pretty
import Type.Util

import Constraint.Exp
import Constraint.Bits

import Desugar.Exp
import Desugar.Slurp.Base
import Desugar.Slurp.State
import Desugar.Slurp.Util
import Desugar.Slurp.SlurpX
import Desugar.Slurp.SlurpS


-----
stage	= "Desugar.Slurp.Slurp"

-- | Slurp out type constraints from this tree.
slurpTreeM ::	Tree Annot1	-> CSlurpM (Tree Annot2, [CTree])
slurpTreeM	tree
 = do
	-- Slurp out type constraints from the tree.
	(tree', qss)	<- liftM unzip $ mapM slurpP tree
	let qs		= concat qss
	
	-- We need to sort the constraints into an order that'll be acceptable to the solver. 
	--	Put all 'known' stuff like external types, sigs, and class definitions at the front
	--	and the constraints for the top level bindings after in one mutually recursive group.
	let [qsLet, qsDef, qsProject, qsDataFields, qsSig]
			= partitionFs [isCBranchLet, isCDef, isCProject, isCDataFields, isCSig]
			$ qs

	let vsLet	= concat
			$ map (\b -> case branchBind b of
				BLet vs	-> vs)
			$ qsLet

	let qsFinal_let	= [CBranch 
				{ branchBind 	= BLetGroup vsLet
				, branchSub	= qsLet }]
				
	let qsFinal	= qsDataFields ++ qsProject ++ qsDef ++ qsSig ++ qsFinal_let

	
	return 	 (tree', qsFinal)
	

-- | Slurp out type constraints from a top level thing.
slurpP 	:: Top Annot1	
	-> CSlurpM (Top Annot2, [CTree])


-- external types
slurpP	(PExtern sp v tv to) 
 = do
	let src		= TSSig sp v

	vT		<- lbindVtoT v
	let qs	= 
		[CDef src vT 	tv]
	
	return	( PExtern Nothing v tv to
		, qs)

-- region/effect/class definitions
slurpP	(PRegion sp v)
 =	return 	(PRegion Nothing v, [])
 
slurpP	(PEffect sp v k)
 =	return	(PEffect Nothing v k, [])
 
slurpP	(PClass	sp v k)
 =	return	(PClass Nothing v k, [])


-- class dictionaries
slurpP top@(PClassDict sp v ts context sigs)
 = do 	
 	qs	<- mapM (\(v, t) 
			 -> do	vT	<- lbindVtoT v
			 	let src	= TSSig sp v
				return	$ CDef src vT t)
			$ sigs

	return	( PClassDict Nothing v ts context sigs
		, qs)
			


slurpP top@(PClassInst sp v ts context exps)
 = do	
	let src	= TSClassInst sp v
 	(exps', css, xsClos)	
 			<- liftM unzip3
	 		$  mapM (\(v, x)
	 			-> do	(xVarT, xEff, xFreeVs, x', cs)	<- slurpX x
					return	$ ((v, x'), cs, xFreeVs))
			$ exps

	return	( PClassInst Nothing v ts context exps'
		, [ CClassInst src v ts ] )

	
-- type Signatures
slurpP	(PSig sp v tSig) 
 = do
	let src		= TSSig sp v
	tVar		<- lbindVtoT v

	let qs	= 
		[CSig src tVar tSig]

 	return	( PSig Nothing v tSig
		, qs)


-- data definitions
slurpP	(PData sp v vs ctors)
 = do
	let src		= TSData sp

 	(ctors', constrss)
			<- liftM unzip
			$  mapM (slurpCtorDef v vs) ctors

	let top'	= PData Nothing v vs ctors'
	addDataDef top'

	-- Build a list of all the named fields from all the constructors
	-- in the object. The constraint solver will need this to work out
	-- the result type for projections.
	--
	let dataFields	= CDataFields src v vs 
			$ catMaybes
			$ map (\df -> case dLabel df of
					Nothing	-> Nothing
					Just l	-> Just (l, dType df))
			$ concat
			$ [fieldDefs	| CtorDef _ _ fieldDefs <- ctors]
			
	return	( top'
		, concat constrss ++ [dataFields])
			

-- projection dictionaries
slurpP	(PProjDict sp t ss)
 = do
	let src		= TSProjDict sp

 	let projVars	= [ (vField, vImpl)
				| SBind _ (Just vField) (XVar _ vImpl) <-  ss]
	
	(_, _, _, ss', _)
			<- liftM unzip5
			$  mapM slurpS ss

	return	( PProjDict Nothing t ss'
		, [CProject src t projVars] )
	
	
-- bindings
slurpP (PBind sp mV x)

 = do
	(vT, eff, clo, stmt', qs)
			<- slurpS (SBind sp mV x)

	-- If the stmt binds a var then we want the type for it from the solver.
	(case bindingVarOfStmt stmt' of
		Just v	
		 -> do	vT_bound	<- getVtoT v
		 	wantTypeV vT_bound
			return ()
		
		Nothing	-> return ())
			
	let (SBind nn mV' x')
			= stmt'
	
	return	( PBind Nothing mV' x'
		, qs )
		

slurpP top
 = 	return 	(PNil, [])
		


-- | Make type schemes for constructors.
--   Slurp out constraints for data field initialisation code.	
slurpCtorDef
	:: Var 					-- Datatype name.
	-> [Var] 				-- Datatype args.
	-> CtorDef Annot1			-- Constructor def.

	-> CSlurpM 	( CtorDef Annot2	-- Annotated constructor def.
			, [CTree] )		-- Schemes and constraints.

slurpCtorDef	vData  vs (CtorDef sp cName fieldDefs)
 = do
	let src		= TSData sp

	TVar _ cNameT	<- bindVtoT cName

	-- Slurp the initialization code from the data fields.
	(fieldDefs', initConstrss)
			<- liftM unzip
			$  mapM (slurpDataField vData cName) fieldDefs
	
	-- Build a constructor type from the data definition.
 	ctorType	<- makeCtorType (vData, vs) (cNameT, fieldDefs')

	-- Add the definition to the ctor table.
	--	The slurper for pattern matching code will need these definitions later on.
	addDef cNameT ctorType


	-- Record what data type this constructor belongs to.
	modify (\s -> s {
		stateCtorType	= Map.insert cName 
					(TData vData (map (\v -> TVar (kindOfSpace $ Var.nameSpace v) v) vs))
				$ stateCtorType s })

	-- Record the fields for this constructor.
	modify (\s -> s { 
		stateCtorFields	= Map.insert cName fieldDefs'
				$ stateCtorFields s })

	let constr = 
		   [ CDef src (TVar KData cNameT) ctorType ]
		++ case initConstrss of
			[]	-> []
			_	-> [newCBranch 
					{ branchSub = concat initConstrss }]
	
	-----
	
	return	( CtorDef Nothing cName fieldDefs'
		, constr )
 	
	
slurpDataField 
	:: Var 					-- Datatype name.
	-> Var					-- Constructor name
	-> DataField (Exp Annot1) Type 		-- DataField def.
	-> CSlurpM 
		( DataField (Exp Annot2) Type	-- Annotated DataField def.
		, [CTree])			-- Constraints for field initialisation code.
	
slurpDataField vData vCtor field
 | Just initX 	<- dInit field
 , Just label	<- dLabel field
 = do
	let src		= TSField vData vCtor label
	tInit		<- newTVarD
	
	(tX, eX, cX, initX', qsX)	
		<- slurpX initX
	
	let qs	=
		[ CEq src tInit tX
		, CEq src tInit (dType field) ] 

	let field'	= 
		DataField 
			{ dPrimary	= dPrimary field
			, dLabel	= dLabel field
			, dType		= dType field
			, dInit		= Just initX' }
	
	return	( field'
		, qs ++ qsX
		)
 	
 | otherwise
 = do
  	let field'	= 
		DataField
			{ dPrimary	= dPrimary field
			, dLabel	= dLabel   field
			, dType		= dType	   field
			, dInit		= Nothing }
			
	return	( field'
		, [])
 
 

