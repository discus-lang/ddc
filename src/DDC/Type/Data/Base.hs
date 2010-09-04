
-- | Data type and constructor definitions.
module DDC.Type.Data.Base
	( DataDef(..)
	, CtorDef(..)
	, lookupTypeOfFieldFromDataDef
	, lookupTypeOfNamedFieldFromCtorDef
	, lookupTypeOfNumberedFieldFromCtorDef
	, lookupLabelOfFieldIndex
	, fieldsOfDataDef
	, fieldTypeLabels)
where
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Operators.Strip
import DDC.Var
import Control.Monad
import Data.List
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
	, dataDefParams	:: [(Var, Kind)]

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

	-- minus one here because the last element corresponds to the
	-- return type of the function, which isn't a parameter.
   in if ix < length tsBits - 1
	 then Just $ makeTForall_front bksForall (tsBits !! ix)
	 else Nothing


-- | Lookup the field label corresponding to the index of the field.
lookupLabelOfFieldIndex :: Int -> CtorDef -> Maybe Var
lookupLabelOfFieldIndex ix ctorDef
	= liftM fst
	$ find (\(_, ix') -> ix == ix')
	$ Map.toList
	$ ctorDefFields ctorDef


-- | Get a set of all fields defined in a data type declaration.
fieldsOfDataDef :: DataDef -> Set Var
fieldsOfDataDef dataDef
	= Set.unions
	$ map (Set.fromList . Map.keys . ctorDefFields)
	$ Map.elems $ dataDefCtors dataDef


-- | Get the list of all field types with optional names in the
--   order they appear in the constructor.
--   The field types are wrapped in the same forall quantifiers as the type
--   of the whole constructor.
fieldTypeLabels :: CtorDef -> [(Maybe Var, Type)]
fieldTypeLabels ctorDef
 =	[ ( lookupLabelOfFieldIndex ix ctorDef
	  , let Just t = lookupTypeOfNumberedFieldFromCtorDef ix ctorDef in t)
	| ix <- [0 .. (ctorDefArity ctorDef - 1)] ]

