
-- | Names used in the Disciple Core Salt language profile.
module DDC.Core.Salt.Name
        ( Name          (..)

          -- * Primitive Type Constructors
        , PrimTyCon     (..)
        , pprPrimTyConStem
        , readPrimTyCon,        readPrimTyConStem
        , primTyConIsIntegral
        , primTyConIsFloating
        , primTyConIsUnsigned
        , primTyConIsSigned
        , primTyConWidth

          -- * Primitive Literals
        , PrimLit       (..)
        , readPrimLit
        
          -- * Primitive Operators
        , PrimOp        (..)
        , readPrimOp

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
        , readLitSize
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

        -- | A primitive literal.
        | NamePrimLit   PrimLit
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
        NamePrimLit   lit       -> rnf lit


instance Pretty Name where
 ppr nn
  = case nn of
        NameVar  n              -> text n
        NameCon  n              -> text n
        NameExt  n ext          -> ppr n <> text "$" <> text ext
        NameObjTyCon            -> text "Obj"
        NamePrimTyCon tc        -> ppr tc
        NamePrimOp p            -> ppr p
        NamePrimLit lit         -> ppr lit


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

        -- PrimOp
        | Just p        <- readPrimOp str
        = Just $ NamePrimOp p

        -- PrimLit
        | Just p        <- readPrimLit str
        = Just $ NamePrimLit p

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
data PrimOp
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


-- | Read a primitive operator.
readPrimOp :: String -> Maybe PrimOp
readPrimOp str
        -- PrimArith
        | Just p        <- readPrimArith str
        = Just $ PrimArith p

        -- PrimCast
        | Just p        <- readPrimCast str
        = Just $ PrimCast p

        -- PrimCall
        | Just p        <- readPrimCall str
        = Just $ PrimCall p

        -- PrimControl
        | Just p        <- readPrimControl str
        = Just $ PrimControl p

        -- PrimStore
        | Just p        <- readPrimStore str
        = Just $ PrimStore p

        | otherwise
        = Nothing


-- PrimLit --------------------------------------------------------------------
-- | Primitive literals.
data PrimLit 
        -- | The void literal.
        = PrimLitVoid

        -- | A boolean literal.
        | PrimLitBool   Bool

        -- | A natural number literal.
        | PrimLitNat    Integer

        -- | An integer number literal.
        | PrimLitInt    Integer

        -- | A size literal.
        | PrimLitSize   Integer

        -- | A word literal, of the given width.
        | PrimLitWord   Integer Int

        -- | A floating point literal, of the given width.
        | PrimLitFloat  Double  Int

        -- | A string literal.
        | PrimLitString Text

        -- | A constructor tag literal.
        | PrimLitTag    Integer
        deriving (Eq, Ord, Show)


instance NFData PrimLit where
 rnf p
  = case p of
        PrimLitVoid             -> ()
        PrimLitBool   b         -> rnf b
        PrimLitNat    i         -> rnf i
        PrimLitInt    i         -> rnf i
        PrimLitSize   i         -> rnf i
        PrimLitWord   i bits    -> rnf i `seq` rnf bits
        PrimLitFloat  f bits    -> rnf f `seq` rnf bits
        PrimLitString bs        -> rnf bs
        PrimLitTag    i         -> rnf i


instance Pretty PrimLit where
 ppr p 
  = case p of
        PrimLitVoid             -> text "V#"
        PrimLitBool True        -> text "True#"
        PrimLitBool False       -> text "False#"
        PrimLitNat   i          -> integer i <> text "#"
        PrimLitInt   i          -> integer i <> text "i#"
        PrimLitSize  i          -> integer i <> text "s#"
        PrimLitWord  i bits     -> integer i <> text "w" <> int bits <> text "#"
        PrimLitFloat f bits     -> double  f <> text "f" <> int bits <> text "#"
        PrimLitString tx        -> (text $ show $ T.unpack tx) <> text "#"
        PrimLitTag   i          -> text "TAG" <> integer i <> text "#"


-- | Read a primitive literal.
readPrimLit :: String -> Maybe PrimLit
readPrimLit str
        -- Literal void
        | str == "V#" 
        = Just $ PrimLitVoid

        -- Literal Bools
        | str == "True#"  = Just $ PrimLitBool True
        | str == "False#" = Just $ PrimLitBool False

        -- Literal Nats
        | Just str'     <- stripSuffix "#" str
        , Just val      <- readLitNat str'
        = Just $ PrimLitNat  val

        -- Literal Ints
        | Just str'     <- stripSuffix "#" str
        , Just val      <- readLitInt str'
        = Just $ PrimLitInt  val

        -- Literal Sizes
        | Just str'     <- stripSuffix "s#" str
        , Just val      <- readLitSize str'
        = Just $ PrimLitSize val

        -- Literal Words
        | Just str'        <- stripSuffix "#" str
        , Just (val, bits) <- readLitWordOfBits str'
        , elem bits [8, 16, 32, 64]
        = Just $ PrimLitWord val bits

        -- Literal Floats
        | Just str'        <- stripSuffix "#" str
        , Just (val, bits) <- readLitFloatOfBits str'
        , elem bits [32, 64]
        = Just $ PrimLitFloat val bits

        -- Literal Tags
        | Just rest     <- stripPrefix "TAG" str
        , (ds, "#")     <- span isDigit rest
        = Just $ PrimLitTag (read ds)

        | otherwise
        = Nothing

