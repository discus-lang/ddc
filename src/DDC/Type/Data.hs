
{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- | Data type declarations.
module DDC.Type.Data
	(CtorDef(..))
where
import DDC.Type.Exp
import DDC.Var
import Data.Map		(Map)


-- | Meta-data about a data constructor.
--	We need to remember the indices of each field so we can convert
--	pattern matches using labels to Sea form. 
data CtorDef
	= CtorDef 
	{ ctorDefName	:: Var 		-- ^ Name of constructor.
	, ctorDefType	:: Type		-- ^ Type of constructor.
	, ctorDefArity	:: Int		-- ^ Arity of constructor.
	, ctorDefTag	:: Int		-- ^ Tag of constructor.
	, ctorDefFields	:: Map Var Int  -- ^ Map of field names to indices in the constructor.
	}
	deriving (Show, Eq)

