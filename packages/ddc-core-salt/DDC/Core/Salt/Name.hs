
-- | Names used in the Disciple Core Salt language profile.
module DDC.Core.Salt.Name
        ( Name          (..)

          -- * Primitive Type Constructors
        , PrimTyCon     (..)
        , readPrimTyCon
        , primTyConIsIntegral
        , primTyConIsFloating
        , primTyConIsUnsigned
        , primTyConIsSigned
        , primTyConWidth

          -- * Primitive Operators
        , PrimOp        (..)

        , PrimArith     (..)
        , readPrimArith

        , PrimCast      (..)
        , readPrimCast
        , primCastPromoteIsValid
        , primCastTruncateIsValid

        , PrimStore     (..)
        , readPrimStore

        , PrimCall      (..)
        , readPrimCall

        , PrimControl   (..)
        , readPrimControl

          -- * Primitive Literals
        , readLitInteger
        , readLitPrimNat
        , readLitPrimInt
        , readLitPrimWordOfBits

          -- * Name Parsing
        , readName

          -- * Name Sanitisation
        , sanitizeName
        , sanitizeGlobal
        , sanitizeLocal)
where
import DDC.Core.Salt.Name.Sanitize
import DDC.Core.Salt.Name.PrimTyCon
import DDC.Core.Salt.Name.PrimOp
import DDC.Core.Salt.Name.Lit
import DDC.Base.Pretty
import Data.Typeable
import Data.Char
import Data.List
import Control.DeepSeq

-- | Names of things used in Disciple Core Salt.
data Name
        -- | A type or value variable.
        = NameVar       String

        -- | Constructor names.
        | NameCon       String

        -- | The abstract heap object type constructor.
        | NameObjTyCon

        -- | A primitive type constructor.
        | NamePrimTyCon PrimTyCon

        -- | A primitive operator.
        | NamePrimOp    PrimOp

        -- | The void literal.
        | NameLitVoid

        -- | A boolean literal.
        | NameLitBool   Bool

        -- | A natural number literal.
        | NameLitNat    Integer

        -- | An integer number literal.
        | NameLitInt    Integer

        -- | A constructor tag literal.
        | NameLitTag    Integer

        -- | A @WordN#@ literal, of the given width.
        | NameLitWord   Integer Int
        deriving (Eq, Ord, Show, Typeable)


instance NFData Name where
 rnf name
  = case name of
        NameVar s               -> rnf s
        NameCon s               -> rnf s
        NameObjTyCon            -> ()
        NamePrimTyCon con       -> rnf con
        NamePrimOp    op        -> rnf op
        NameLitVoid             -> ()
        NameLitBool   b         -> rnf b
        NameLitNat    i         -> rnf i
        NameLitInt    i         -> rnf i
        NameLitTag    i         -> rnf i
        NameLitWord   i bits    -> rnf i `seq` rnf bits


instance Pretty Name where
 ppr nn
  = case nn of
        NameVar  n              -> text n
        NameCon  n              -> text n
        NameObjTyCon            -> text "Obj"
        NamePrimTyCon tc        -> ppr tc
        NamePrimOp p            -> ppr p
        NameLitVoid             -> text "V#"
        NameLitBool True        -> text "True#"
        NameLitBool False       -> text "False#"
        NameLitNat  i           -> integer i  <> text "#"
        NameLitInt  i           -> integer i  <> text "i#"
        NameLitTag  i           -> text "TAG" <> integer i <> text "#"
        NameLitWord i bits      -> integer i <> text "w" <> int bits <> text "#"


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
        | str == "V#" 
        = Just $ NameLitVoid

        -- Literal Nats
        | Just val <- readLitPrimNat str
        = Just $ NameLitNat  val

        -- Literal Ints
        | Just val <- readLitPrimInt str
        = Just $ NameLitInt  val

        -- Literal Tags
        | Just rest     <- stripPrefix "TAG" str
        , (ds, "#")     <- span isDigit rest
        = Just $ NameLitTag (read ds)

        -- Literal Bools
        | str == "True#"  = Just $ NameLitBool True
        | str == "False#" = Just $ NameLitBool False

        -- Literal Words
        | Just (val, bits) <- readLitPrimWordOfBits str
        , elem bits [8, 16, 32, 64]
        = Just $ NameLitWord val bits

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

