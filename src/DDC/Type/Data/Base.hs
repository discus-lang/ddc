
-- | Data type and constructor definitions.
module DDC.Type.Data.Base
	( DataDef(..)
	, CtorDef(..)
	, lookupTypeOfFieldFromDataDef
	, lookupTypeOfNamedFieldFromCtorDef
	, lookupTypeOfNumberedFieldFromCtorDef
	, fieldsOfDataDef)
where
import DDC.Type.Strip
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Var
import Data.Maybe
import Data.Set			(Set)
import Data.Map			(Map)
import qualified Data.Map	as Map
import qualified Data.Set	as Set


-- | A data type definition
data DataDef
	= DataDef
	{ -- | Name of the type constructor.
	  dataDefName	:: Var

	  -- | Parameter variables to the data type.
	, dataDefParams	:: [Var]

	  -- | Map of data constructor name to definition.
	, dataDefCtors	:: Map Var CtorDef }
	deriving (Show, Eq)


-- | A data constructor definition.
--	We need to remember the indices of each field so we can convert
--	pattern matches using labels to Sea form. 
data CtorDef
	= CtorDef 
	{ -- | Name of the data constructor.
	  ctorDefName	:: Var

	  -- | Type of the data constructor.
	, ctorDefType	:: Type

	  -- | Arity of the constructor (number of parameters).
	, ctorDefArity	:: Int

	  -- | Tag of the constructor (order in the data type decl).
	, ctorDefTag	:: Int

	  -- | Map of field names to indices in the constructor.
	, ctorDefFields	:: Map Var Int
	}
	deriving (Show, Eq)


-- | Get the type of a named field from a data type definition.
--	If multiple constructors define this field then we just take the
--	type of the first one. It's up to the constructor of the `DataDef` 
--	to ensure that all fields in a def have the same type.
lookupTypeOfFieldFromDataDef :: Var -> DataDef -> Maybe Type
lookupTypeOfFieldFromDataDef v def
	= listToMaybe
	$ catMaybes
	$ map (lookupTypeOfNamedFieldFromCtorDef v) 
	$ Map.elems 
	$ dataDefCtors def


-- | Get the type of a named field from a data constructor definition.
--   The argument type is wrapped in the same forall quantifiers as the type
--   of the whole constructor.
lookupTypeOfNamedFieldFromCtorDef :: Var -> CtorDef -> Maybe Type
lookupTypeOfNamedFieldFromCtorDef vCtor ctorDef
 = case Map.lookup vCtor $ ctorDefFields ctorDef of
	Just ix	-> lookupTypeOfNumberedFieldFromCtorDef ix ctorDef
	_	-> Nothing


-- | Get the type of a numbered field from a data constructor definition.
--   The argument type is wrapped in the same forall quantifiers as the type
--   of the whole constructor.
lookupTypeOfNumberedFieldFromCtorDef :: Int -> CtorDef -> Maybe Type
lookupTypeOfNumberedFieldFromCtorDef ix ctorDef
 = let	(bksForall, [], tBody)	
		= stripForallContextT $ ctorDefType ctorDef
		
	tsBits	= flattenTFuns tBody

   in if length tsBits <= ix
	 then Nothing
	 else Just $ makeTForall_front bksForall (tsBits !! ix)


-- | Get a set of all fields defined in a data type declaration.
fieldsOfDataDef :: DataDef -> Set Var
fieldsOfDataDef dataDef
	= Set.unions
	$ map (Set.fromList . Map.keys . ctorDefFields)
	$ Map.elems $ dataDefCtors dataDef





