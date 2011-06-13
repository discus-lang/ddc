{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Data type and constructor definitions.
module DDC.Type.Data.Base
	( DataDef(..)
	, dataDefKind
	, CtorDef(..)
	, lookupTypeOfFieldFromDataDef
	, lookupTypeOfNamedFieldFromCtorDef
	, lookupTypeOfNumberedFieldFromCtorDef
	, lookupLabelOfFieldIndex
	, fieldsOfDataDef
	, fieldTypeLabels

	, Materiality(..)
--	, combineMateriality
--	, contextualiseMateriality
	, paramMaterialityOfDataDef)
--	, paramMaterialityOfFun)

where
import DDC.Type.Builtin
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Operators.Strip
import DDC.Var
import DDC.Main.Pretty
import Control.Monad
import Data.List
import Data.Maybe
import Data.Set			(Set)
import Data.Map			(Map)
import qualified Data.Map	as Map
import qualified Data.Set	as Set


-- DataDef ----------------------------------------------------------------------------------------
-- | A data type definition
data DataDef
	= DataDef
	{ -- | Name of the type constructor.
	  dataDefName		:: Var

	  -- | Optional Sea name for the type constructor,
	  --   used when importing an external data type.
	, dataDefSeaName	:: Maybe String

	  -- | Parameter variables to the data type.
	, dataDefParams		:: [(Var, Kind)]

	  -- | Map of data constructor name to definition.
	, dataDefCtors		:: Map Var CtorDef

	  -- | Cache of the material type vars of the data type.
	  --   These correspond to object that may appear in the store.
	, dataDefMaterialVars   :: Maybe (Set Var)

	  -- | Cache of immaterial type vars in the data type.
	  --   These are ''phantom'' variables, and an object of this type
	  --   will never appear in the store. Like the @a@ and @b@ in  @a -> b@.
	, dataDefImmaterialVars :: Maybe (Set Var)
	}
	deriving (Show, Eq)


-- | Get the kind of a data type constructor from its definition.
dataDefKind :: DataDef -> Kind
dataDefKind ddef
	= makeKFuns (map snd $ dataDefParams ddef) kValue


-- CtorDef ----------------------------------------------------------------------------------------
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

	  -- | The type of each field in the constructor.
	, ctorDefFieldTypes :: [Type]
	}
	deriving (Show, Eq)


-- Fields -----------------------------------------------------------------------------------------
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


-- Materiality ------------------------------------------------------------------------------------
--
-- | Describes how a parameter variable of the data type is used
--   in the constructors of its definition.
data Materiality
	-- | Completely absent from the constructors.
	= MaterialAbsent

	-- | Variable either has a kind that is always immaterial (effect or closure)
	--   or is only used in an immaterial position, like in the return type of
	--   a function.
	| MaterialNot

	-- | Variable is only used a material position. If the type we're talking about
	--   is a value or region type then this means ``Strongly'' material, whereas if
	--   it's a closure type it's ``Weakly'' material.
--	| MaterialHas
	| MaterialStrong

	-- | Variable is used in both a material and immaterial position.
	| MaterialMixed
	deriving (Eq, Show)

instance Pretty Materiality PMode where
 ppr mat	= ppr $ show mat

{-
-- | Compute the resulting materiality when the same variable has multiple occurrences.
--   This is the l.u.b over the following order:
--
--   @
--           Mixed
--        Has      Not
--           Absent
--   @
--
combineMateriality :: Materiality -> Materiality -> Materiality
combineMateriality m1 m2
 = case (m1, m2) of
	(MaterialAbsent, _)			-> m2
	(_,              MaterialAbsent)	-> m1

	(MaterialMixed, _)			-> MaterialMixed
	(_             , MaterialMixed)		-> MaterialMixed

	(MaterialStrong, MaterialStrong)	-> MaterialStrong
	(MaterialNot,    MaterialNot)		-> MaterialNot

	(MaterialStrong, MaterialNot)		-> MaterialMixed
	(MaterialNot,    MaterialStrong)	-> MaterialMixed


-- | For a type like (Int %r1 -> Int %r2), even though %r1 is strongly material
--   in Int %r1, in the function type it is in an immaterial context, so is
--   immaterial in the overall function type. g.l.b over the following order:
--
--   @
--            Has
--           Mixed
--            Not
--           Absent
--   @
--
contextualiseMateriality :: Materiality -> Materiality -> Materiality
contextualiseMateriality m1 m2
 = case (m1, m2) of
	(MaterialAbsent, _)			-> MaterialAbsent
	(_,              MaterialAbsent)	-> MaterialAbsent

	(MaterialNot,    _)			-> MaterialNot
	(_,              MaterialNot)		-> MaterialNot

	(MaterialMixed, _)			-> MaterialMixed
	(_,		 MaterialMixed)		-> MaterialMixed

	(MaterialStrong, MaterialStrong)	-> MaterialStrong
-}

-- | Get a list giving the materiality of each of the parameter variables
--   of a given data type.
paramMaterialityOfDataDef :: DataDef -> Maybe [Materiality]
paramMaterialityOfDataDef dataDef
 | Just mvs	<- dataDefMaterialVars  dataDef
 , Just ivs	<- dataDefImmaterialVars dataDef
 = let	materiality1 v
	 = case (Set.member v mvs, Set.member v ivs) of
		(False, False)	-> MaterialAbsent
		(False, True)	-> MaterialNot
		(True,  False)	-> MaterialStrong
		(True,  True)	-> MaterialMixed

   in	Just $ map (materiality1 . fst) $ dataDefParams dataDef

 | otherwise
 = Nothing

-- paramMaterialityOfFun
--	= [MaterialNot, MaterialNot, MaterialNot, MaterialIs]

