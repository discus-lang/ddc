{-# OPTIONS -fno-warn-monomorphism-restriction #-}

module DDC.Type.Data.Elaborate
	(elaborateDataDef)
where
import Core.Pretty		()
import DDC.Type.Data.Base
import DDC.Type.Data.CtorType
import DDC.Type
import DDC.Var
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Base.DataFormat
import Util.Data.List
import Control.Monad
import qualified Shared.VarPrim	as Var
import qualified Data.Map	as Map
import qualified Debug.Trace

stage		= "DDC.Type.Data.Elaborate"
debug		= False
trace ss xx	= if debug then Debug.Trace.trace (pprStrPlain ss) xx else xx

-- | Elaborate a data type declaration.
--   This uses the kind of each type constructor to fill in missing region variables.
--   TODO: This only works on one declaration at a time, so it won't propertly
--         elaborate recursive data types. We also want to merge kind inference with
--         the elaboration process.
--
elaborateDataDef
	:: Monad m
	=> (NameSpace 	-> m Var)		-- ^ Function to allocate a fresh variable.
	-> (Var		-> m Kind)		-- ^ Function to get the kind of a type variable.
	-> DataDef
	-> m DataDef

elaborateDataDef newVarN getKind 
	dataDef@(DataDef vData vksParam ctorDefs _ _)
 = do

	-- Decide what var to use as the primary region.
	--	If the first parameter to the type constructor is a region variable
	--	then use that, otherwise make a fresh one.
	rPrimary	<- case takeHead vksParam of
				Just (v, k)
				  | isRegionKind k
				  -> return v
				
				_ -> newVarN NameRegion

	-- Hack the existing variable generator function to return the primary
	-- region variable for all region variables.
	let newVarNR n 
		| n == NameRegion
		= return rPrimary
		
		| otherwise
		= newVarN n

	let result
		-- TUnit doesn't have a region
		| vData == Var.primTUnit 
		= return dataDef
	
		-- Unboxed types, except for String# don't have regions
		| Var.varIsUnboxedTyConData vData
		, vData /= Var.primTString Unboxed
		= return dataDef
		
		-- boxed ones do
		| otherwise
		= do	
			-- Elaborate the parameter types of all the constructors.
			-- This may allocate fresh region variables that need to be 
			-- added to the resulting data type.
			(ctorDefsElabs, vkssNew)	
				<- liftM unzip 
				$  mapM (elaborateCtorDefParams newVarNR) 
				$  Map.elems ctorDefs

			let vksParam_elab
				= nub $ (rPrimary, kRegion) : vksParam ++ (concat vkssNew)
					
			ctorDefs_final
				<- mapM (elaborateCtorDefResult newVarNR vData vksParam_elab)
					ctorDefsElabs
				
			let dataDef_final
				= dataDef
				{ dataDefParams	= vksParam_elab
				, dataDefCtors	= Map.fromList 
						$ [(ctorDefName def, def) | def <- ctorDefs_final] }

			trace 	( "elaborateData\n"
				% "    in:\n" 	%> dataDef		% "\n"
				% "    out:\n"	%> dataDef_final	% "\n")
				$ return ()

			return	dataDef_final			
	result


-- CtorDef ----------------------------------------------------------------------------------------
-- | Holds information about a data constructor while we're elaborating it.
--   We can't build a complete CtorDef because during elaboration we won't 
--   know what region variables are supposed to be on the returned data type.
data CtorDefElab
	= CtorDefElab
	{ ctorDefElabOrig	:: CtorDef
	, ctorDefElabParamTypes	:: [Type] }
	deriving (Show)
	

-- | Elaborate a data constructor definiton.
elaborateCtorDefParams
	:: Monad m
	=> (NameSpace -> m Var)		-- ^ A function to generate fresh region varibles.
	-> CtorDef			-- ^ Constructor definition to elaborate.
	-> m 	( CtorDefElab		--   partially elaborated constructor definition
		, [(Var, Kind)])	--   the new variables added, with their kinds.

elaborateCtorDefParams newVarN ctorDef
 = do
	-- The type in the provided CtorDef might have alreay been elaborated
	-- and have foralls and contexts on the front.
	-- TODO: we're currently not handling type class context on data constructors.
	let (_, [], tBody)
	 	= stripForallContextT $ ctorDefType ctorDef 
		
	-- Take the parameter types from the given constructor type.
	-- We can ignore the return type as makeCtorType will add it back.
	let tsBits		= flattenTFuns tBody
	let Just tsParams	= takeInit tsBits

	-- Elaborate each of the parameter types individually.
	(tsParams_elab, vkssNew)
			<- liftM unzip
			$  mapM (\t -> do 	
					(tR, vksNewR)	<- elaborateRsT newVarN t
					(tE, vksNewE)	<- annotEffClo  newVarN tR
					return (tE, vksNewR ++ vksNewE))
				tsParams
		
	let ctorDefElab
		= CtorDefElab
		{ ctorDefElabOrig	= ctorDef
		, ctorDefElabParamTypes	= tsParams_elab }
		
 	return	( ctorDefElab
		, concat vkssNew )


-- | Once we know what the parameters of the return type should be we can
--   rebuild the complete types of the data constructors.
elaborateCtorDefResult
	:: Monad m
	=> (NameSpace -> m Var)
	-> Var				-- ^ Var of of the data type constructor
	-> [(Var, Kind)]			-- ^ Params to the data type constructor.
	-> CtorDefElab			-- ^ Partially elaborated constructor def			
	-> m CtorDef
	
elaborateCtorDefResult newVarN vData vksParam ctorDef
 = do	tCtor_elab	<- makeCtorType 
				newVarN 
				vData (map fst vksParam)
				(ctorDefName $ ctorDefElabOrig ctorDef)
				(ctorDefElabParamTypes ctorDef)

	return	$ (ctorDefElabOrig ctorDef)
			{ ctorDefType	= tCtor_elab }


-- | Ensure that all function constructors are annotated with
--   effect and closure variables.
annotEffClo 
	:: Monad m
	=> (NameSpace -> m Var)
	-> Type
	-> m (Type, [(Var, Kind)])

annotEffClo newVarN tt
 = case tt of
	TVar{}	-> return (tt, [])
	TCon{}	-> return (tt, [])
	TSum{}	-> return (tt, [])
	
	TApp tX tY
	 | Just (t1, t2, eff, clo)	<- takeTFun tt
	 -> do	(t1',  vs1) <- annotEffClo newVarN t1
		(t2',  vs2) <- annotEffClo newVarN t2
		(eff', vs3) <- makeAnnot   newVarN eff
		(clo', vs4) <- makeAnnot   newVarN clo
		return	( makeTFun t1' t2' eff' clo'
			, vs1 ++ vs2 ++ vs3 ++ vs4)
	
	 | otherwise
	 -> do	(t1', vs1)  <- annotEffClo newVarN tX
		(t2', vs2)  <- annotEffClo newVarN tY
		return	( TApp t1' t2'
			, vs1 ++ vs2)
			
	_	-> panic stage 
		$ "annotEffClo: no match"
	
	
-- | If this is a bottom effect or closure then replace it with
--   a fresh variable, and return that variable and it's kind.	
makeAnnot
	:: Monad m
	=> (NameSpace -> m Var)
	-> Type
	-> m (Type, [(Var, Kind)])
	
makeAnnot newVarN tt
 = case tt of
	TSum k []
	 | isEffectKind k
	 -> do	vEff	<- newVarN NameEffect
		return	( TVar k (UVar vEff)
			, [(vEff, kEffect)])

	 | isClosureKind k
	 -> do	vClo	<- newVarN NameClosure
		return	( TVar k (UVar vClo)
			, [(vClo, kClosure)])
		
	_ 	-> return (tt, [])
