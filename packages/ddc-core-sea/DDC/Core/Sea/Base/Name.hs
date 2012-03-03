
-- | Names used by the Sea core language profile.
--   Some of the primop types are also used by the SeaOutput profile.
module DDC.Core.Sea.Base.Name
        ( Name          (..)
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

        -- | A primitive type constructor.
        | NamePrimTyCon PrimTyCon

        -- | A primitive data constructor.
        | NamePrimDaCon PrimDaCon

        -- | A primitive operator.
        | NamePrimOp    PrimOp

        -- | A integer literal.
        | NameInt       Integer
        deriving (Eq, Show)


-- PrimTyCon -----------------------------------------------------------------
-- | Primitive type constructors.
data PrimTyCon
        -- Unboxed data types ---------
        -- | Type of machine addresses.
        = PrimTyConAddr

        -- | Unsigned words of the given length.
        | PrimTyConWord   Int

        -- | Signed integers of the given length.
        | PrimTyConInt    Int

        -- | Floating point numbers of the given length.
        | PrimTyConFloat  Int

        -- Algebraic data types -------
        | PrimTyConUnit         -- ^ Unit type constructor.
        | PrimTyConPair         -- ^ @Pair@ data constructor.
        | PrimTyConList         -- ^ @List@ type constructor.
        deriving (Show, Eq)


-- PrimDaCon ------------------------------------------------------------------
data PrimDaCon
        = PrimDaConUnit         -- ^ Unit data constructor (@()@).
        | PrimDaConPr           -- ^ @Pr@ data construct (pairs).
        | PrimDaConNil          -- ^ @Nil@ data constructor.
        | PrimDaConCons         -- ^ @Cons@ data constructor.
        deriving (Show, Eq, Ord)


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
        deriving (Show, Eq)


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
