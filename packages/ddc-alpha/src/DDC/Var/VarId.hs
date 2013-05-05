{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# OPTIONS -O2 #-}

-- | Variable binding identifiers.
module DDC.Var.VarId
	( VarId				(..)
	, takeDataFormatOfVarId
	, incVarId )
where
import DDC.Var.PrimId
import DDC.Base.DataFormat
import DDC.Main.Pretty
import Data.Hashable

-- | A variable binding identifier gives a variable its unique identity.
data VarId
	-- | A regular user-defined var.
	= VarId     !String !Int

	-- | A primitive variable, which as special meaning to the compiler.
	| VarIdPrim !PrimId

	-- | Variable id not set. Before being processed by the renamer most variables will have this.
	| VarIdNil
	deriving (Show)


instance Eq VarId where
 {-# INLINE (==) #-}
 (==) (VarId s1 i1) (VarId s2 i2)
	| i1 == i2			= s1 == s2
	| otherwise			= False

 (==) (VarIdPrim p1) (VarIdPrim p2)	= p1 == p2
 (==) VarIdNil       VarIdNil           = True
 (==) _ _				= False


instance Ord VarId where
 {-# INLINE compare #-}
 compare (VarId s1 i1) (VarId s2 i2)
	| i1 < i2	= LT
	| i1 > i2	= GT
	| s1 < s2	= LT
	| s1 > s2	= GT
	| otherwise	= EQ

 compare (VarIdPrim p1) (VarIdPrim p2)	= compare p1 p2
 compare VarIdPrim{}    VarId{}		= LT
 compare VarId{}        VarIdPrim{} 	= GT
 compare VarIdNil       VarIdNil        = EQ
 compare VarIdNil       _               = LT
 compare _              VarIdNil        = GT


instance Hashable VarId where
 hashWithSalt s (VarId _ int)           = hashWithSalt s int
 hashWithSalt s (VarIdPrim pid)         = hashWithSalt s pid
 hashWithSalt _ VarIdNil		= 0
 {-# INLINE hashWithSalt #-}
 

instance Pretty VarId PMode where
 ppr b
  = case b of
  	VarId s i	-> ppr $ s ++ show i
	_		-> ppr $ show b
	

-- | Take the embedded DataFormat from a VarId, if any.
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


-- | Increment a VarId to the next one.
incVarId :: VarId -> VarId
{-# INLINE incVarId #-}
incVarId b
 = case b of
 	VarId s i	-> VarId s (i + 1)
	_		-> error $ "incVarId: cannot increment " ++ show b
