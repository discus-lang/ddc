
module DDC.Core.Sea.Lite.Name
        ( Name(..) )
where
import DDC.Core.Sea.Base.Name
import DDC.Base.Pretty


-- | Names of things used in Disciple-Core-Lite.
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

