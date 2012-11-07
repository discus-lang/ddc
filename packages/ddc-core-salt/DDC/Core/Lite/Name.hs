
module DDC.Core.Lite.Name
        ( Name          (..) 
        , DataTyCon     (..)
        , PrimDaCon     (..)
        , readName)
where
import DDC.Core.Salt.Name.PrimTyCon
import DDC.Core.Salt.Name.PrimOp
import DDC.Core.Salt.Name.Lit
import DDC.Base.Pretty
import Data.Typeable
import Data.Char


-- | Names of things used in Disciple Core Lite.
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

        -- | A primitive arithmetic operator.
        | NamePrimArith PrimArith

        -- | An Unboxed boolean literal
        | NameBool      Bool

        -- | An unboxed natural literal.
        | NameNat       Integer

        -- | An unboxed integer literal.
        | NameInt       Integer

        -- | An unboxed word literal
        | NameWord      Integer Int

        deriving (Eq, Ord, Show, Typeable)


instance Pretty Name where
 ppr nn
  = case nn of
        NameVar  v              -> text v
        NameCon  c              -> text c
        NameDataTyCon dc        -> ppr dc
        NamePrimTyCon tc        -> ppr tc
        NamePrimDaCon dc        -> ppr dc
        NamePrimArith op        -> ppr op
        NameBool True           -> text "True#"
        NameBool False          -> text "False#"
        NameNat  i              -> integer i <> text "#"
        NameInt  i              -> integer i <> text "i" <> text "#"
        NameWord i bits         -> integer i <> text "w" <> int bits <> text "#"


-- | Read the name of a variable, constructor or literal.
readName :: String -> Maybe Name
readName str
        |  Just name    <- readDataTyCon str
        =  Just $ NameDataTyCon name

        |  Just name    <- readPrimTyCon str
        =  Just $ NamePrimTyCon name

        |  Just name    <- readPrimDaCon str
        =  Just $ NamePrimDaCon name

        -- PrimArith
        | Just p        <- readPrimArith str
        = Just $ NamePrimArith p

        -- Literal unit value.
        | str == "()"
        = Just $ NamePrimDaCon PrimDaConUnit

        -- Literal Bools
        | str == "True#"  = Just $ NameBool True
        | str == "False#" = Just $ NameBool False

        -- Literal Nat
        | Just val <- readLitPrimNat str
        = Just $ NameNat  val

        -- Literal Ints
        | Just val <- readLitPrimInt str
        = Just $ NameInt  val

        -- Literal Words
        | Just (val, bits) <- readLitPrimWordOfBits str
        , elem bits [8, 16, 32, 64]
        = Just $ NameWord val bits


        -- Constructors.
        | c : _         <- str
        , isUpper c
        = Just $ NameCon str

        -- Variables.
        | c : _         <- str
        , isLower c      
        = Just $ NameVar str

        | otherwise
        = Nothing



-- DataTyCon ------------------------------------------------------------------
-- | Baked-in data type constructors.
data DataTyCon
        = DataTyConUnit         -- ^ @Unit@  type constructor.
        | DataTyConBool         -- ^ @Bool@  type constructor.
        | DataTyConNat          -- ^ @Nat@   type constructor.
        | DataTyConInt          -- ^ @Int@   type constructor.
        | DataTyConPair         -- ^ @Pair@  type constructor.
        | DataTyConList         -- ^ @List@  type constructor.
        deriving (Eq, Ord, Show)


instance Pretty DataTyCon where
 ppr dc
  = case dc of
        DataTyConUnit           -> text "Unit"
        DataTyConBool           -> text "Bool"
        DataTyConNat            -> text "Nat"
        DataTyConInt            -> text "Int"
        DataTyConPair           -> text "Pair"
        DataTyConList           -> text "List"


-- | Read a Baked-in data type constructor.
readDataTyCon :: String -> Maybe DataTyCon
readDataTyCon str
 = case str of
        "Unit"  -> Just DataTyConUnit
        "Bool"  -> Just DataTyConBool
        "Nat"   -> Just DataTyConNat
        "Int"   -> Just DataTyConInt
        "Pair"  -> Just DataTyConPair
        "List"  -> Just DataTyConList
        _       -> Nothing


-- PrimDaCon ------------------------------------------------------------------
-- | Baked-in data constructors.
data PrimDaCon
        = PrimDaConUnit         -- ^ Unit   data constructor (@()@).
        | PrimDaConBoolU        -- ^ @B#@   data constructor.
        | PrimDaConNatU         -- ^ @N#@   data constructor.
        | PrimDaConIntU         -- ^ @I#@   data constructor.
        | PrimDaConPr           -- ^ @Pr@   data construct (pairs).
        | PrimDaConNil          -- ^ @Nil@  data constructor (lists).
        | PrimDaConCons         -- ^ @Cons@ data constructor (lists).
        deriving (Show, Eq, Ord)


instance Pretty PrimDaCon where
 ppr dc
  = case dc of
        PrimDaConBoolU          -> text "B#"
        PrimDaConNatU           -> text "N#"
        PrimDaConIntU           -> text "I#"

        PrimDaConUnit           -> text "()"
        PrimDaConPr             -> text "Pr"
        PrimDaConNil            -> text "Nil"
        PrimDaConCons           -> text "Cons"


-- | Read a Baked-in data constructor.
readPrimDaCon :: String -> Maybe PrimDaCon
readPrimDaCon str
 = case str of
        "B#"    -> Just PrimDaConBoolU
        "N#"    -> Just PrimDaConNatU
        "I#"    -> Just PrimDaConIntU

        "()"    -> Just PrimDaConUnit
        "Pr"    -> Just PrimDaConPr
        "Nil"   -> Just PrimDaConNil
        "Cons"  -> Just PrimDaConCons
        _       -> Nothing

