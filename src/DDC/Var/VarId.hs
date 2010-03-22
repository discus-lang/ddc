
-- | Variable binding identifiers.
module DDC.Var.VarId
	( VarId				(..)
	, takeDataFormatOfVarId
	, incVarId )
where
import DDC.Var.PrimId
import DDC.Base.DataFormat
import DDC.Main.Pretty


-- | A variable binding identifier.
--	This gives the variable its unique identity.
data VarId
	-- A regular user-defined var.
	= VarId     String Int

	-- A primitive variable, which as special meaning to the compiler.
	| VarIdPrim PrimId

	-- binding not set
	| VarIdNil
	deriving (Show, Eq, Ord)
	

instance Pretty VarId PMode where
 ppr b
  = case b of
  	VarId s i	-> ppr $ s ++ show i
	_		-> ppr $ show b
	

-- | If this varBind contains an embedded DataFormat, then Just it
takeDataFormatOfVarId :: VarId -> Maybe DataFormat
takeDataFormatOfVarId vid
 = case vid of
 	VarIdPrim (TBool   fmt)	-> Just fmt
	VarIdPrim (TWord   fmt)	-> Just fmt
	VarIdPrim (TInt    fmt)	-> Just fmt
	VarIdPrim (TFloat  fmt)	-> Just fmt
	VarIdPrim (TChar   fmt)	-> Just fmt
	VarIdPrim (TString fmt)	-> Just fmt
	_			-> Nothing


-- | Increment a regular VarId to the next one
incVarId :: VarId -> VarId
incVarId b
 = case b of
 	VarId s i	-> VarId s (i + 1)
	_		-> error $ "incVarId: cannot increment " ++ show b
