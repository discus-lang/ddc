
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

          -- * Primitive Arithmetic
        , PrimArith     (..)
        , readPrimArith

          -- * Primitive Calls
        , PrimCall      (..)
        , readPrimCall

          -- * Primitive Casts
        , PrimCast      (..)
        , readPrimCast
        , primCastPromoteIsValid
        , primCastTruncateIsValid

          -- * Primitive Control
        , PrimControl   (..)
        , readPrimControl

          -- * Primitive Store
        , PrimStore     (..)
        , readPrimStore

          -- * Primitive Vector
        , PrimVec       (..)
        , readPrimVec
        , multiOfPrimVec
        , liftPrimArithToVec
        , lowerPrimVecToArith
    
          -- * Primitive Literals
        , readLitInteger
        , readLitNat
        , readLitInt
        , readLitWordOfBits
        , readLitFloatOfBits

          -- * Name Parsing
        , readName
        
        , takeNameVar )
where
import DDC.Core.Salt.Name.PrimArith
import DDC.Core.Salt.Name.PrimCall
import DDC.Core.Salt.Name.PrimCast
import DDC.Core.Salt.Name.PrimControl
import DDC.Core.Salt.Name.PrimStore
import DDC.Core.Salt.Name.PrimTyCon
import DDC.Core.Salt.Name.PrimVec
import DDC.Core.Salt.Name.Lit
import DDC.Core.Lexer.Names             (isVarStart)
import DDC.Data.ListUtils
import DDC.Base.Pretty
import DDC.Base.Name
import Data.Typeable
import Data.Char
import Data.List
import Control.DeepSeq
import Data.Text                        (Text)
import qualified Data.Text              as T


-- | Names of things used in Disciple Core Salt.
data Name
        -- | A type or value variable.
        = NameVar       String

        -- | Constructor names.
        | NameCon       String

        -- | An extended name.
        | NameExt       Name String

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

        -- | A size literal.
        | NameLitSize   Integer

        -- | A word literal, of the given width.
        | NameLitWord   Integer Int

        -- | A floating point literal, of the given width.
        | NameLitFloat  Double  Int

        -- | A string literal.
        | NameLitString Text

        -- | A constructor tag literal.
        | NameLitTag    Integer
        deriving (Eq, Ord, Show, Typeable)


instance NFData Name where
 rnf name
  = case name of
        NameVar s               -> rnf s
        NameExt n s             -> rnf n `seq` rnf s
        NameCon s               -> rnf s
        NameObjTyCon            -> ()
        NamePrimTyCon con       -> rnf con
        NamePrimOp    op        -> rnf op
        NameLitVoid             -> ()
        NameLitBool   b         -> rnf b
        NameLitNat    i         -> rnf i
        NameLitInt    i         -> rnf i
        NameLitSize   i         -> rnf i
        NameLitWord   i bits    -> rnf i `seq` rnf bits
        NameLitFloat  f bits    -> rnf f `seq` rnf bits
        NameLitString bs        -> rnf bs
        NameLitTag    i         -> rnf i


instance Pretty Name where
 ppr nn
  = case nn of
        NameVar  n              -> text n
        NameCon  n              -> text n
        NameExt  n ext          -> ppr n <> text "$" <> text ext
        NameObjTyCon            -> text "Obj"
        NamePrimTyCon tc        -> ppr tc
        NamePrimOp p            -> ppr p
        NameLitVoid             -> text "V#"
        NameLitBool True        -> text "True#"
        NameLitBool False       -> text "False#"
        NameLitNat   i          -> integer i <> text "#"
        NameLitInt   i          -> integer i <> text "i#"
        NameLitSize  i          -> integer i <> text "s#"
        NameLitWord  i bits     -> integer i <> text "w" <> int bits <> text "#"
        NameLitFloat f bits     -> double  f <> text "f" <> int bits <> text "#"
        NameLitString tx        -> (text $ show $ T.unpack tx) <> text "#"
        NameLitTag   i          -> text "TAG" <> integer i <> text "#"


instance CompoundName Name where
 extendName n str       
  = NameExt n str
 
 splitName nn
  = case nn of
        NameExt n str   -> Just (n, str)
        _               -> Nothing


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

        -- Literal Bools
        | str == "True#"  = Just $ NameLitBool True
        | str == "False#" = Just $ NameLitBool False

        -- Literal Nats
        | Just str'     <- stripSuffix "#" str
        , Just val      <- readLitNat str'
        = Just $ NameLitNat  val

        -- Literal Ints
        | Just str'     <- stripSuffix "#" str
        , Just val      <- readLitInt str'
        = Just $ NameLitInt  val

        -- Literal Sizes
        | Just str'     <- stripSuffix "s#" str
        , Just val      <- readLitSize str'
        = Just $ NameLitSize val

        -- Literal Words
        | Just str'        <- stripSuffix "#" str
        , Just (val, bits) <- readLitWordOfBits str'
        , elem bits [8, 16, 32, 64]
        = Just $ NameLitWord val bits

        -- Literal Floats
        | Just str'        <- stripSuffix "#" str
        , Just (val, bits) <- readLitFloatOfBits str'
        , elem bits [32, 64]
        = Just $ NameLitFloat val bits

        -- Literal Tags
        | Just rest     <- stripPrefix "TAG" str
        , (ds, "#")     <- span isDigit rest
        = Just $ NameLitTag (read ds)

        -- Constructors.
        | c : _         <- str
        , isUpper c      
        = Just $ NameVar str

        -- Variables.
        | c : _         <- str
        , isVarStart c  || c == '_'
        = Just $ NameVar str

        | otherwise
        = Nothing


-- | Take the string of a non-primitive name. Supports extended names.
takeNameVar :: Name -> Maybe String

takeNameVar (NameVar n)
    = Just n

takeNameVar (NameExt n str)
    | Just n' <- takeNameVar n
    = Just (n' ++ "$" ++ str)

takeNameVar _
    = Nothing


-- PrimOp ---------------------------------------------------------------------
-- | Primitive operators implemented directly by the machine or runtime system.
data    PrimOp
        -- | Arithmetic, logic, comparison and bit-wise operators.
        = PrimArith     PrimArith

        -- | Casting between numeric types.
        | PrimCast      PrimCast

        -- | Raw store access.
        | PrimStore     PrimStore

        -- | Special function calling conventions.
        | PrimCall      PrimCall

        -- | Non-functional control flow.
        | PrimControl   PrimControl
        deriving (Eq, Ord, Show)


instance NFData PrimOp where
 rnf op
  = case op of
        PrimArith pa    -> rnf pa
        PrimCast  pc    -> rnf pc
        PrimStore ps    -> rnf ps
        PrimCall  pc    -> rnf pc
        PrimControl pc  -> rnf pc


instance Pretty PrimOp where
 ppr pp
  = case pp of
        PrimArith    op -> ppr op
        PrimCast     c  -> ppr c
        PrimStore    p  -> ppr p
        PrimCall     c  -> ppr c
        PrimControl  c  -> ppr c

