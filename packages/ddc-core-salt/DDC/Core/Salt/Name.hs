
-- | Names used in the Disciple Core Salt language profile.
--   These map directly onto names used in the C output language.
module DDC.Core.Salt.Name
        ( Name            (..), readName
        , sanitizeName
        , sanitizeGlobal
        , sanitizeLocal
        , module DDC.Core.Salt.Name.Prim
        , module DDC.Core.Salt.Name.Lit)
where
import DDC.Core.Salt.Name.Sanitize
import DDC.Core.Salt.Name.Prim
import DDC.Core.Salt.Name.Lit
import DDC.Base.Pretty
import Data.Typeable
import Data.Char
import Data.List


-- | Names of things used in Disciple Core Salt.
data Name
        -- | A type or value variable
        = NameVar       String

        -- | We still need a constructor-like name for modules.
        | NameCon       String

        -- | The object type constructor.
        | NameObjTyCon

        -- | A type primitive constructor.
        | NamePrimTyCon PrimTyCon

        -- | A primitive operator.
        | NamePrimOp    PrimOp

        -- | The void literal.
        | NameVoid

        -- | A boolean literal.
        | NameBool      Bool

        -- | A natural number literal.
        | NameNat       Integer

        -- | An integer number literal.
        | NameInt       Integer

        -- | A WordN literal, of the given width.
        | NameWord      Integer Int

        -- | A constructor tag literal.
        | NameTag       Integer
        deriving (Eq, Ord, Show, Typeable)


instance Pretty Name where
 ppr nn
  = case nn of
        NameVar  n        -> text n
        NameCon  n        -> text n
        NameObjTyCon      -> text "Obj"
        NamePrimTyCon tc  -> ppr tc
        NamePrimOp p      -> ppr p
        NameVoid          -> text "V#"
        NameNat  i        -> integer i  <> text "#"
        NameInt  i        -> integer i  <> text "i#"
        NameTag  i        -> text "TAG" <> integer i <> text "#"
        NameBool True     -> text "True#"
        NameBool False    -> text "False#"
        NameWord i bits   -> integer i <> text "w" <> int bits <> text "#"


-- | Read the name of a variable, constructor or literal.
readName :: String -> Maybe Name
readName str
        -- Obj 
        | str == "Obj"
        = Just $ NameObjTyCon

        -- PrimTyCon
        | Just p        <- readPrimTyCon str
        = Just $ NamePrimTyCon p

        -- PrimArith
        | Just p        <- readPrimArith str
        = Just $ NamePrimOp $ PrimArith p

        -- PrimCast
        | Just p        <- readPrimCast str
        = Just $ NamePrimOp $ PrimCast p

        -- PrimCall
        | Just p        <- readPrimCall str
        = Just $ NamePrimOp $ PrimCall p

        -- PrimControl
        | Just p        <- readPrimControl str
        = Just $ NamePrimOp $ PrimControl p

        -- PrimStore
        | Just p        <- readPrimStore str
        = Just $ NamePrimOp $ PrimStore p

        -- Literal void
        | str == "V#" = Just $ NameVoid

        -- Literal Nats
        | Just val <- readLitPrimNat str
        = Just $ NameNat  val

        -- Literal Ints
        | Just val <- readLitPrimInt str
        = Just $ NameInt  val

        -- Literal Tags
        | Just rest     <- stripPrefix "TAG" str
        , (ds, "#")     <- span isDigit rest
        = Just $ NameTag (read ds)

        -- Literal Bools
        | str == "True#"  = Just $ NameBool True
        | str == "False#" = Just $ NameBool False


        -- Literal Words
        | Just (val, bits) <- readLitPrimWordOfBits str
        , elem bits [8, 16, 32, 64]
        = Just $ NameWord val bits

        -- Constructors.
        | c : _         <- str
        , isUpper c      
        = Just $ NameVar str

        -- Variables.
        | c : _         <- str
        , isLower c      
        = Just $ NameVar str

        | otherwise
        = Nothing

