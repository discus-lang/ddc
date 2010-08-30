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
import DDC.Base.DataFormat
import Util.Data.List
import Control.Monad
import qualified Shared.VarPrim	as Var
import qualified Data.Map	as Map
import qualified Debug.Trace

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
	dataDef@(DataDef vData vsParam ctorDefs)
 = do

	-- Decide what var to use as the primary region.
	--	If the first parameter to the type constructor is a region variable
	--	then use that, otherwise make a fresh one.
	rPrimary	<- case takeHead vsParam of
				Just v
				  | varNameSpace v == NameRegion
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

			let vsParam_elab
				= nub $ rPrimary : vsParam ++ (map fst $ concat vkssNew)
					
			ctorDefs_final
				<- mapM (elaborateCtorDefResult newVarNR vData vsParam_elab)
					ctorDefsElabs
				
			let dataDef_final
				= dataDef
				{ dataDefCtors	= Map.fromList 
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
			$  mapM (elaborateRsT newVarN) tsParams
		
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
	-> [Var]			-- ^ Params to the data type constructor.
	-> CtorDefElab			-- ^ Partially elaborated constructor def			
	-> m CtorDef
	
elaborateCtorDefResult newVarN vData vsParam ctorDef
 = do	tCtor_elab	<- makeCtorType 
				newVarN 
				vData vsParam
				(ctorDefName $ ctorDefElabOrig ctorDef)
				(ctorDefElabParamTypes ctorDef)

	return	$ (ctorDefElabOrig ctorDef)
			{ ctorDefType	= tCtor_elab }

