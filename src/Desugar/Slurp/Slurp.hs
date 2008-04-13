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
import qualified Shared.VarUtil	as Var

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import Shared.Base
import Shared.Exp

import Type.Exp
import Type.Location
import Type.Pretty
import Type.Util
import Type.Plate

import Constraint.Exp
import Constraint.Bits

import Desugar.Exp
import Desugar.Slurp.Base
import Desugar.Slurp.State
import Desugar.Slurp.Util
import Desugar.Slurp.SlurpX
import Desugar.Slurp.SlurpS


-----
-- stage	= "Desugar.Slurp.Slurp"

-- | Slurp out type constraints from this tree.
slurpTreeM ::	Tree Annot1	-> CSlurpM (Tree Annot2, [CTree], Set Var)
slurpTreeM	tree
 = do
	-- sort the top level things so that data definitions go through before their uses.
	let psSorted	
		= partitionFsSort
			[ (=@=) PRegion{}, 	(=@=) PEffect{}, 	(=@=) PData{}
			, (=@=) PClass{},	(=@=) PClassDict{}
			, (=@=) PImport{},	(=@=) PExtern{}
			, (=@=) PProjDict{},	(=@=) PClassInst{}
			, (=@=) PSig{}
			, (=@=) PBind{} ]
			tree

	-- Slurp out type constraints from the tree.
	(tree', qss)	<- liftM unzip $ mapM slurpP psSorted
	let qs		= concat qss
	

	-- pack all the bindings together.
	let (qsBranch, qsRest)
		= partition isCBranch qs

	let vsLet	= concat
			$ map (\b -> case branchBind b of
				BLet vs	-> vs
				_	-> [])
			$ qsBranch

	let qsFinal_let	= [CBranch 
				{ branchBind 	= BLetGroup vsLet
				, branchSub	= qsBranch }]
				
	-- Sort tthe constraints into an order acceptable by the solver.
	let qsFinal_rest
		= partitionFsSort
			[ (=@=) CDataFields{}, (=@=) CProject{}, (=@=) CClassInst{}
			, (=@=) CDef{}
			, (=@=) CSig{} ]
			qsRest
			
	let qsFinal = qsFinal_rest ++ qsFinal_let
	
	return	( tree'
	 	, qsFinal
		, Set.fromList vsLet)


-- | Slurp out type constraints from a top level thing.
slurpP 	:: Top Annot1	
	-> CSlurpM (Top Annot2, [CTree])


-- external types
slurpP	(PExtern sp v tv to) 
 = do
	vT		<- lbindVtoT v
	let qs	= 
		[CDef (TSV $ SVSigExtern sp v) vT 	tv]
	
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
slurpP top@(PClassDict sp vClass tsParam context sigs)
 = do 	
	-- create a signature from each of the bindings in the class definition
	-- eg: for something like
	--
	--	class Num a where
	--	 (+) :: a -> a -> a
	--
	-- the actual type of (+) is
	--
	-- 	(+) :: forall a. Num a => a -> a -> a
	--
	let makeDef (vSig, tSig)
	     	= do 	vT		<- lbindVtoT vSig

			-- add a forall for each of the parameters of the type class
	     		let vsParam	= map (\(TVar _ v) -> v) tsParam
			let TForall vks t = makeTForall (zip vsParam (repeat KData)) tSig

			-- add the enclosing class constraint
			let tSig'	= TForall vks 
					$ addFetters_front [FConstraint vClass tsParam] t

			return $ CDef 	(TSV $ SVSigClass sp vClass) vT tSig'

	qs <- mapM makeDef sigs

	return	( PClassDict Nothing vClass tsParam context sigs
		, qs)
			


slurpP top@(PClassInst sp v ts context ss)
 = do	
	-- All the RHS of the statements are vars, so we don't get any useful constraints back
	(_, _, _, ss', _)
			<- liftM unzip5
			$  mapM slurpS ss


	return	( PClassInst Nothing v ts context ss'
		, [ CClassInst (TSM $ SMClassInst sp v) v ts ] )

	
-- type Signatures
slurpP	(PSig sp v tSig) 
 = do
	tVar		<- lbindVtoT v

	let qs	= 
		[CSig (TSV $ SVSig sp v) tVar tSig]

 	return	( PSig Nothing v tSig
		, qs)


-- data definitions
slurpP	(PData sp v vs ctors)
 = do
 	(ctors', constrss)
			<- liftM unzip
			$  mapM (slurpCtorDef v vs) ctors

	let top'	= PData Nothing v vs ctors'
	addDataDef top'

	-- Build a list of all the named fields from all the constructors
	-- in the object. The constraint solver will need this to work out
	-- the result type for projections.
	--
	let dataFields	= CDataFields (TSM $ SMData sp) v vs 
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
 	let projVars	= [ (vField, vImpl)
				| SBind _ (Just vField) (XVar _ vImpl) <-  ss]

	-- All the RHS of the statements are vars, so we don't get any useful constraints back
	(_, _, _, ss', _)
			<- liftM unzip5
			$  mapM slurpS ss

	return	( PProjDict Nothing t ss'
		, [CDictProject (TSM $ SMProjDict sp) t (Map.fromList projVars)] )
	
	
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
			
	let (SBind _ mV' x')
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
	Just (TVar _ cNameT)	
			<- bindVtoT cName

	-- Slurp the initialization code from the data fields.
	(fieldDefs', initConstrss)
			<- liftM unzip
			$  mapM (slurpDataField sp vData cName) fieldDefs
	
	-- Build a constructor type from the data definition.
 	ctorType	<- makeCtorType newVarN vData vs cNameT fieldDefs'
	
	-- Add the definition to the ctor table.
	--	The slurper for pattern matching code will need these definitions later on.
	addDef cNameT ctorType


	-- Record what data type this constructor belongs to.
	let kData	= makeDataKind vs
	let tsData	= map (\v -> TVar (kindOfSpace $ Var.nameSpace v) v) vs
	modify (\s -> s {
		stateCtorType	= Map.insert cName 
					(TData kData vData tsData)
				$ stateCtorType s })

	-- Record the fields for this constructor.
	modify (\s -> s { 
		stateCtorFields	= Map.insert cName fieldDefs'
				$ stateCtorFields s })

	let constr = 
		   [ CDef (TSV $ SVCtorDef sp vData cName) (TVar kData cNameT) ctorType ]
		++ case concat initConstrss of
			[]	-> []
			_	-> [CBranch 
					{ branchBind = BNil
					, branchSub = concat initConstrss }]
	
	-----
	
	return	( CtorDef Nothing cName fieldDefs'
		, constr )
 	
	
slurpDataField 
	:: SourcePos
	-> Var 					-- Datatype name.
	-> Var					-- Constructor name
	-> DataField (Exp Annot1) Type	 	-- DataField def.
	-> CSlurpM 
		( DataField (Exp Annot2) Type	-- Annotated DataField def.
		, [CTree])			-- Constraints for field initialisation code.
	
slurpDataField sp vData vCtor field
 | Just initX 	<- dInit field
 , Just label	<- dLabel field
 = do
	tInit		<- newTVarD

	-- vars for the initialisation function
	tInitFun	<- newTVarD
	rInit		<- newTVarR
	
	(tX, eX, cX, initX', qsX)	
		<- slurpX initX

	tField_fresh	<- freshenType $ dType field
	
	let qs	=
		[ CEq (TSV $ SVCtorField sp vData vCtor label) 
			tX (TFun tInitFun tField_fresh pure empty) ]

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


-- | Replace non-ctor variables in this type by fresh ones.
freshenType 
	:: Type
	-> CSlurpM Type
	
freshenType tt
 = do	let vsFree	= freeVars tt
 	let vsFree'	= filter (\v -> (not $ Var.isCtorName v)) $ Set.toList vsFree
	let tsFree'	= map (\v -> TVar (kindOfSpace $ Var.nameSpace v) v) vsFree'
	
	vsFresh		<- mapM newVarZ vsFree'
	let tsFresh	= map (\v -> TVar (kindOfSpace $ Var.nameSpace v) v) vsFresh
	
	let sub		= Map.fromList $ zip tsFree' tsFresh

	let tt'		= subTT_noLoops sub tt
	return	tt'
						



