{-# LANGUAGE ParallelListComp #-}

-- | This module is NOT to be used DDC proper -- for unit testing only.
--   It uses `unsafePerformIO` under the covers to generate fresh variables.
--
--   Easy creation of data type definitions.
--
module DDC.Test.Data
	( module DDC.Type.Data
	, createCtorType
	, createCtorDef
	, createDataDef)
where
import DDC.Var
import DDC.Type
import DDC.Type.Data
import DDC.Type.Data.CtorType
import System.IO.Unsafe
import DDC.Test.Var
import qualified Data.Map	as Map


-- | Filthily create constructor type.
createCtorType
	:: Var 		-- ^ Name of data type constructor.
	-> [Var]	-- ^ Parameters to data type constructor.
	-> Var		-- ^ Name of data constructor.
	-> [Type]	-- ^ Parameters to data constructor.
	-> Type

createCtorType vData vsData vCtor vsCtor
	= unsafePerformIO $ makeCtorType newVarIO vData vsData vCtor vsCtor


-- | Filthily create a `CtorDef`
createCtorDef
	:: Var		-- ^ Name of type constructor
	-> [Var]	-- ^ Parameters to type constructor.
	-> Int		-- ^ Tag of constructor
	-> Var		-- ^ Name of data constructor
	-> [Type]	-- ^ Parameters to data constructor
	-> CtorDef

createCtorDef vData vsData tag vCtor tsCtor
	= CtorDef
	{ ctorDefName		= vCtor
	, ctorDefType		= createCtorType vData vsData vCtor tsCtor
	, ctorDefArity		= length tsCtor
	, ctorDefTag		= tag
	, ctorDefFields		= Map.empty }


-- | Filthily create a `DataDef`
createDataDef
	:: Var
	-> [Type]		-- ^ Parameters to data constructor, must be TVars.
	-> [(Var, [Type])]
	-> DataDef

createDataDef vData tsData cs
 = let	Just vksData	= sequence
			$ [ case t of
				TVar k (UVar v)	-> Just (v, k)
				_		-> Nothing
			  | t <- tsData]
   in 	 DataDef
	{ dataDefName		= vData
	, dataDefSeaName	= Nothing
	, dataDefParams		= vksData
	, dataDefCtors
		= Map.fromList
		$ [(vCtor, createCtorDef vData (map fst vksData) tag vCtor tsArgs)
			| (vCtor, tsArgs) 	<- cs
			| tag			<- [0..] ]

	, dataDefMaterialVars	= Nothing
	, dataDefImmaterialVars	= Nothing }
