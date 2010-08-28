
{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- | Data type declarations.
module DDC.Type.Data
	( DataDef(..)
	, CtorDef(..))
where
import DDC.Type.Exp
import DDC.Var
import Data.Map		(Map)


-- | A data type definition
data DataDef
	= DataDef
	{ -- | Name of the type constructor.
	  dataDefName	:: Var

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


-- | Get a list of all the parameters of a data constructor's type, 
--   retaining the outer quantifiers. 
quantParamsOfCtorType :: Type -> [Type]
quantParamsOfCtorType t
	= quantParamsOfCtorType' [] t
	
quantParamsOfCtorType acc tt
	| 

