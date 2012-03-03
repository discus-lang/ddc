
-- | Names used by the Sea core language profile.
--   Some of the primop types are also used by the SeaOutput profile.
module DDC.Core.Sea.Base.Name
        ( Name          (..)
        , DataTyCon     (..)
        , PrimTyCon     (..)
        , PrimOp        (..)
        , primOpNames)
where
import DDC.Base.Pretty


-- Names of things used in the Sea core language profile.
data Name
        -- | User defined variables.
        = NameVar       String

        -- | A user defined constructor.
        | NameCon       String

        -- | Baked in data type constructors.
        | NameDataTyCon DataTyCon

        -- | A primitive type constructor.
        | NamePrimTyCon PrimTyCon

        -- | A primitive data constructor.
        | NamePrimDaCon PrimDaCon

        -- | A primitive operator.
        | NamePrimOp    PrimOp

        -- | A integer literal.
        | NameInt       Integer
        deriving (Eq, Ord, Show)


instance Pretty Name where
 ppr nn
  = case nn of
        NameVar  v              -> text v
        NameCon  c              -> text c
        NameDataTyCon dc        -> ppr dc
        NamePrimTyCon tc        -> ppr tc
        NamePrimDaCon dc        -> ppr dc
        NamePrimOp op           -> ppr op
        NameInt i               -> text (show i)


-- PrimTyCon -----------------------------------------------------------------
-- | Primitive type constructors.
data PrimTyCon
        -- | Type of store pointers
        = PrimTyConPtr

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
        deriving (Eq, Ord, Show)


instance Pretty PrimTyCon where
 ppr tc
  = case tc of
        PrimTyConPtr            -> text "Ptr#"
        PrimTyConAddr           -> text "Addr#"
        PrimTyConNat            -> text "Nat#"
        PrimTyConTag            -> text "Tag#"
        PrimTyConBool           -> text "Bool#"
        PrimTyConWord  bits     -> text "Word"  <> int bits <> text "#"
        PrimTyConInt   bits     -> text "Int"   <> int bits <> text "#"
        PrimTyConFloat bits     -> text "Float" <> int bits <> text "#"


-- DataTyCon ------------------------------------------------------------------
data DataTyCon
        = DataTyConUnit         -- ^ Unit type constructor.
        | DataTyConPair         -- ^ @Pair@ data constructor.
        | DataTyConList         -- ^ @List@ type constructor.
        deriving (Eq, Ord, Show)


instance Pretty DataTyCon where
 ppr dc
  = case dc of
        DataTyConUnit           -> text "Unit"
        DataTyConPair           -> text "Pair"
        DataTyConList           -> text "List"


-- PrimDaCon ------------------------------------------------------------------
data PrimDaCon
        = PrimDaConUnit         -- ^ Unit data constructor (@()@).
        | PrimDaConPr           -- ^ @Pr@ data construct (pairs).
        | PrimDaConNil          -- ^ @Nil@ data constructor.
        | PrimDaConCons         -- ^ @Cons@ data constructor.
        deriving (Show, Eq, Ord)

instance Pretty PrimDaCon where
 ppr dc
  = case dc of
        PrimDaConUnit           -> text "Unit"
        PrimDaConPr             -> text "Pr"
        PrimDaConNil            -> text "Nil"
        PrimDaConCons           -> text "Cons"


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

