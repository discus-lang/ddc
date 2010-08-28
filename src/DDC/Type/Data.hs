
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

{-
-- | Get a list of all the parameters of a data constructor's type, retaining the outer quantifiers. 
--   This doesn't support constrained types. If there are any constraints then `panic`.
quantParamsOfCtorType :: Type -> [Type]
quantParamsOfCtorType t
	= quantParamsOfCtorType' [] t
	
quantParamsOfCtorType' bksQuant acc tt

	-- Remember quantified vars when we see them.
	| TForall b k t			<- tt
	= quantParamsOfCtorType'
		(bksQuant ++ [(b, k)])
		acc
		t

	-- We've got a function constructor, so add its param to the accumulator.
	-- Also wrap it with the current set of quantified vars.
	| Just (t1, t2, eff, clo)	<- takeTFun tt
	= quantParamsOfCtorType'
		bksQuant
		(makeTForall_front 
-}	

