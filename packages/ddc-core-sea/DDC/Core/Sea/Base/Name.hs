
-- | Names used by the Sea core language profile.
--   Some of the primop types are also used by the SeaOutput profile.
module DDC.Core.Sea.Base.Name
        ( PrimTyCon     (..)
        , PrimOp        (..)
        , primOpNames)
where
import DDC.Base.Pretty


-- PrimTyCon -----------------------------------------------------------------
-- | Primitive type constructors.
data PrimTyCon
        -- | The Void type.
        = PrimTyConVoid

        -- | Type of store pointers
        | PrimTyConPtr

        -- | Type of machine addresses.
        | PrimTyConAddr

        -- | Type of natural numbers,
        --   Used for field indices and general counters.
        | PrimTyConNat

        -- | Type of data type tags.
        | PrimTyConTag

        -- | Type of booleans.
        | PrimTyConBool

        -- | Unsigned words of the given length.
        | PrimTyConWord   Int

        -- | Signed integers of the given length.
        | PrimTyConInt    Int

        -- | Floating point numbers of the given length.
        | PrimTyConFloat  Int

        -- | String of UTF8 characters.
        | PrimTyConString 
        deriving (Eq, Ord, Show)


instance Pretty PrimTyCon where
 ppr tc
  = case tc of
        PrimTyConVoid           -> text "Void#"
        PrimTyConPtr            -> text "Ptr#"
        PrimTyConAddr           -> text "Addr#"
        PrimTyConNat            -> text "Nat#"
        PrimTyConTag            -> text "Tag#"
        PrimTyConBool           -> text "Bool#"
        PrimTyConWord   bits    -> text "Word"  <> int bits <> text "#"
        PrimTyConInt    bits    -> text "Int"   <> int bits <> text "#"
        PrimTyConFloat  bits    -> text "Float" <> int bits <> text "#"
        PrimTyConString         -> text "String#"



-- PrimOp ---------------------------------------------------------------------
-- | Primitive numeric, comparison or logic operators.
--   We expect the backend/machine to be able to implement these directly.
data PrimOp
        -- arithmetic
        = PrimOpNeg
        | PrimOpAdd
        | PrimOpSub
        | PrimOpMul
        | PrimOpDiv
        | PrimOpMod

        -- comparison
        | PrimOpEq
        | PrimOpNeq
        | PrimOpGt
        | PrimOpGe
        | PrimOpLt
        | PrimOpLe

        -- boolean
        | PrimOpAnd
        | PrimOpOr
        deriving (Eq, Ord, Show)


instance Pretty PrimOp where
 ppr op 
  = let Just str        = lookup op primOpNames
    in  text str


-- | Names of primitve operators.
primOpNames :: [(PrimOp, String)]
primOpNames
 =      [ (PrimOpNeg, "neg#")
        , (PrimOpAdd, "add#")
        , (PrimOpSub, "sub#")
        , (PrimOpMul, "mul#")
        , (PrimOpDiv, "div#")
        , (PrimOpMod, "mod#")
        , (PrimOpEq , "eq#" )
        , (PrimOpNeq, "neq#")
        , (PrimOpGt , "gt#" )
        , (PrimOpLt , "lt#" )
        , (PrimOpLe , "le#" )
        , (PrimOpAnd, "and#")
        , (PrimOpOr , "or#" ) ]

