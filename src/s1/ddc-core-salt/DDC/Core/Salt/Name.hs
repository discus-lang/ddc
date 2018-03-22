{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

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

          -- * Primitive Values
        , PrimVal       (..)
        , readPrimVal

        , pattern NamePrimOp
        , pattern NamePrimLit

          -- * Primitive Operators
        , PrimOp        (..)
        , readPrimOp

          -- * Primitive Arithmetic
        , PrimArith     (..)
        , readPrimArith

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
        , PrimLit       (..)
        , readPrimLit
        , K.readLitInteger
        , K.readLitNat
        , K.readLitInt
        , K.readLitSize
        , K.readLitWordOfBits
        , K.readLitFloatOfBits

        , pattern NameLitVoid
        , pattern NameLitBool
        , pattern NameLitNat
        , pattern NameLitInt
        , pattern NameLitSize
        , pattern NameLitWord
        , pattern NameLitFloat
        , pattern NameLitChar
        , pattern NameLitTextLit
        , pattern NameLitTag

          -- * Name Parsing
        , readName
        , takeNameVar

          -- * Name Sanitization
        , seaNameOfSuper
        , seaNameOfLocal
        , sanitizeName)
where
import qualified DDC.Core.Module.Import as C
import qualified DDC.Core.Module.Export as C
import DDC.Core.Module

import DDC.Core.Salt.Name.PrimArith
import DDC.Core.Salt.Name.PrimCast
import DDC.Core.Salt.Name.PrimControl
import DDC.Core.Salt.Name.PrimStore
import DDC.Core.Salt.Name.PrimTyCon
import DDC.Core.Salt.Name.PrimVec
import DDC.Type.Exp.Simple.Exp
import DDC.Data.ListUtils
import DDC.Data.Pretty
import DDC.Data.Name
import Control.DeepSeq
import Data.Typeable
import Data.Char
import Data.List
import Data.Maybe
import Data.Text                                        (Text)
import qualified DDC.Core.Codec.Text.Lexer.Tokens       as K
import qualified Data.Text                              as T
import qualified Data.Monoid                            as T
import qualified Data.List                              as L


-- | Names of things used in Disciple Core Salt.
data Name
        -- | A type or value variable.
        = NameVar       !Text

        -- | Constructor names.
        | NameCon       !Text

        -- | An extended name.
        | NameExt       !Name !Text

        -- | The abstract heap object type constructor.
        | NameObjTyCon

        -- | A primitive type constructor.
        | NamePrimTyCon !PrimTyCon

        -- | A primitive value.
        | NamePrimVal   !PrimVal
        deriving (Eq, Ord, Show, Typeable)

instance NFData Name where
 rnf name
  = case name of
        NameVar s               -> rnf s
        NameExt n s             -> rnf n `seq` rnf s
        NameCon s               -> rnf s
        NameObjTyCon            -> ()
        NamePrimTyCon con       -> rnf con
        NamePrimVal   val       -> rnf val


instance Pretty Name where
 ppr nn
  = case nn of
        NameVar  n              -> text $ T.unpack n
        NameCon  n              -> text $ T.unpack n
        NameExt  n ext          -> ppr n <> text "$" <> text (T.unpack ext)
        NameObjTyCon            -> text "Obj"
        NamePrimTyCon tc        -> ppr tc
        NamePrimVal   val       -> ppr val


instance CompoundName Name where
 extendName n str
  = NameExt n $ T.pack str

 newVarName str
  = NameVar $ T.pack str

 splitName nn
  = case nn of
        NameExt n str   -> Just (n, T.unpack str)
        _               -> Nothing


-- | Read the name of a variable, constructor or literal.
readName :: Text -> Maybe Name
readName str
        -- Obj
        | str == "Obj"
        = Just $ NameObjTyCon

        -- PrimTyCon
        | Just p        <- readPrimTyCon $ T.unpack str
        = Just $ NamePrimTyCon p

        -- PrimVal
        | Just p        <- readPrimVal $ T.unpack str
        = Just $ NamePrimVal p

        -- Constructors.
        | Just (c, _)   <- T.uncons str
        , isUpper c
        = Just $ NameVar str

        -- Variables.
        | Just (c, _)   <- T.uncons str
        , K.isVarStart c  || c == '_'
        = Just $ NameVar str

        | otherwise
        = Nothing


-- | Take the string of a non-primitive name. Supports extended names.
takeNameVar :: Name -> Maybe Text

takeNameVar (NameVar n)
    = Just n

takeNameVar (NameExt n str)
    | Just n' <- takeNameVar n
    = Just (n' T.<> "$" T.<> str)

takeNameVar _
    = Nothing


-- PrimVal --------------------------------------------------------------------
-- | Primitive values, meaning both operators and literals.
data PrimVal
        = PrimValOp     !PrimOp
        | PrimValLit    !PrimLit
        deriving (Eq, Ord, Show)

pattern NamePrimOp op   = NamePrimVal (PrimValOp op)
pattern NamePrimLit lit = NamePrimVal (PrimValLit lit)


instance NFData PrimVal where
 rnf p
  = case p of
        PrimValOp op    -> rnf op
        PrimValLit lit  -> rnf lit


instance Pretty PrimVal where
 ppr p
  = case p of
        PrimValOp op    -> ppr op
        PrimValLit lit  -> ppr lit


-- | Read a primitive value.
readPrimVal :: String -> Maybe PrimVal
readPrimVal str
        | Just op       <- readPrimOp str
        = Just $ PrimValOp op

        | Just lit      <- readPrimLit str
        = Just $ PrimValLit lit

        | otherwise
        = Nothing


-- PrimOp ---------------------------------------------------------------------
-- | Primitive operators implemented directly by the machine or runtime system.
data PrimOp
        -- | Arithmetic, logic, comparison and bit-wise operators.
        = PrimArith     !PrimArith

        -- | Casting between numeric types.
        | PrimCast      !PrimCast

        -- | Raw store access.
        | PrimStore     !PrimStore

        -- | Function call and control flow.
        | PrimControl   !PrimControl
        deriving (Eq, Ord, Show)


instance NFData PrimOp where
 rnf op
  = case op of
        PrimArith pa    -> rnf pa
        PrimCast  pc    -> rnf pc
        PrimStore ps    -> rnf ps
        PrimControl pc  -> rnf pc


instance Pretty PrimOp where
 ppr pp
  = case pp of
        PrimArith    op -> ppr op
        PrimCast     c  -> ppr c
        PrimStore    p  -> ppr p
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
        | PrimLitBool    !Bool

        -- | A natural number literal.
        | PrimLitNat     !Integer

        -- | An integer number literal.
        | PrimLitInt     !Integer

        -- | A size literal.
        | PrimLitSize    !Integer

        -- | A word literal, of the given width.
        | PrimLitWord    !Integer !Int

        -- | A floating point literal, of the given width.
        | PrimLitFloat   !Double  !Int

        -- | A character literal.
        | PrimLitChar    !Char

        -- | A text literal.
        | PrimLitTextLit !Text

        -- | A constructor tag literal.
        | PrimLitTag     !Integer
        deriving (Eq, Ord, Show)


pattern NameLitVoid        = NamePrimVal (PrimValLit PrimLitVoid)
pattern NameLitBool    x   = NamePrimVal (PrimValLit (PrimLitBool    x))
pattern NameLitNat     x   = NamePrimVal (PrimValLit (PrimLitNat     x))
pattern NameLitInt     x   = NamePrimVal (PrimValLit (PrimLitInt     x))
pattern NameLitSize    x   = NamePrimVal (PrimValLit (PrimLitSize    x))
pattern NameLitWord    x s = NamePrimVal (PrimValLit (PrimLitWord    x s))
pattern NameLitFloat   x s = NamePrimVal (PrimValLit (PrimLitFloat   x s))
pattern NameLitChar    x   = NamePrimVal (PrimValLit (PrimLitChar    x))
pattern NameLitTextLit x   = NamePrimVal (PrimValLit (PrimLitTextLit x))
pattern NameLitTag     x   = NamePrimVal (PrimValLit (PrimLitTag     x))



instance NFData PrimLit where
 rnf p
  = case p of
        PrimLitVoid             -> ()
        PrimLitBool    b        -> rnf b
        PrimLitNat     i        -> rnf i
        PrimLitInt     i        -> rnf i
        PrimLitSize    i        -> rnf i
        PrimLitWord    i bits   -> rnf i `seq` rnf bits
        PrimLitFloat   f bits   -> rnf f `seq` rnf bits
        PrimLitChar    c        -> rnf c
        PrimLitTextLit bs       -> rnf bs
        PrimLitTag     i        -> rnf i


instance Pretty PrimLit where
 ppr p
  = case p of
        PrimLitVoid             -> text "V#"
        PrimLitBool True        -> text "True#"
        PrimLitBool False       -> text "False#"
        PrimLitNat     i        -> integer i <> text "#"
        PrimLitInt     i        -> integer i <> text "i#"
        PrimLitSize    i        -> integer i <> text "s#"
        PrimLitWord    i bits   -> integer i <> text "w" <> int bits    <> text "#"
        PrimLitFloat   f bits   -> double  f <> text "f" <> int bits    <> text "#"
        PrimLitChar    c        -> text (show c)                        <> text "#"
        PrimLitTextLit tx       -> (text $ show $ T.unpack tx)          <> text "#"
        PrimLitTag     i        -> text "TAG" <> integer i              <> text "#"


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
        , Just val      <- K.readLitNat str'
        = Just $ PrimLitNat  val

        -- Literal Ints
        | Just str'     <- stripSuffix "#" str
        , Just val      <- K.readLitInt str'
        = Just $ PrimLitInt  val

        -- Literal Sizes
        | Just str'     <- stripSuffix "s#" str
        , Just val      <- K.readLitSize str'
        = Just $ PrimLitSize val

        -- Literal Words
        | Just str'        <- stripSuffix "#" str
        , Just (val, bits) <- K.readLitWordOfBits str'
        , elem bits [8, 16, 32, 64]
        = Just $ PrimLitWord val bits

        -- Literal Floats
        | Just str'         <- stripSuffix "#" str
        , Just (val, mbits) <- K.readLitFloatOfBits str'
        = case mbits of
                Just 32 -> Just $ PrimLitFloat val 32
                Just 64 -> Just $ PrimLitFloat val 64
                _       -> Nothing

        -- Literal Tags
        | Just rest     <- stripPrefix "TAG" str
        , (ds, "#")     <- span isDigit rest
        = Just $ PrimLitTag (read ds)

        | otherwise
        = Nothing


-------------------------------------------------------------------------------
-- | Convert the Salt name of a supercombinator to a name we can use when
--   defining the C function.
seaNameOfSuper
        :: Maybe (ImportValue Name (Type Name)) -- ^ How the super is imported
        -> Maybe (ExportValue Name (Type Name)) -- ^ How the super is exported
        -> Name                                 -- ^ Name of the super.
        -> Maybe Doc

seaNameOfSuper mImport mExport nm

        -- Super is defined in this module and not exported.
        | Nothing               <- mImport
        , Nothing               <- mExport
        , Just str              <- takeNameVar nm
        = Just $ text $ sanitizeName $ T.unpack str

        -- Special case for the main function.
        -- This comes with a export definition but we don't want
        -- the 'Main' module name in the symbol.
        -- Super is exported to other modules.
        | Just ev@ExportValueLocal{} <- mExport
        , ModuleName ps         <- C.exportValueLocalModuleName ev
        , Just str              <- takeNameVar nm
        , ps  == ["Main"]
        , str == "main"
        = Just $ text $ T.unpack str

        -- Super is exported to other modules.
        | Just ev@ExportValueLocal{} <- mExport
        , ModuleName ps         <- C.exportValueLocalModuleName ev
        , Just str              <- takeNameVar nm
        = Just $ text $ sanitizeName (L.intercalate "." ps ++ "." ++ T.unpack str)

        -- Super is imported from another module.
        | Just iv@ImportValueModule{} <- mImport
        , ModuleName ps         <- C.importValueModuleName iv
        , Just str              <- takeNameVar nm
        = Just $ text $ sanitizeName (L.intercalate "." ps ++ "." ++ T.unpack str)

        -- Super is imported from C-land and not exported.
        | Just (ImportValueSea _ strSea _) <- mImport
        , Nothing               <- mExport
        = Just $ text $ T.unpack strSea

        -- ISSUE #320: Handle all the import/export combinations.
        --
        -- We don't handle the other cases because we would need to
        -- produce a wrapper to convert the names.
        | Just str                      <- takeNameVar nm
        = Just $ text $ T.unpack str

        | otherwise
        = Nothing


-- | Convert the Salt name of a local variable to a name we can use in the
--   body of a C function.
seaNameOfLocal :: Name -> Maybe Doc
seaNameOfLocal nn
 = case takeNameVar nn of
        Just str -> Just $ text $ "_" ++ sanitizeName (T.unpack str)
        _        -> Nothing


-- Sanitize -------------------------------------------------------------------
-- | Rewrite a name to make it safe to export as an external C symbol.
--
--   Names containing unfriendly characters like '&' are prefixed with '_sym_'
--   and the '&' is replaced by 'ZAn'. Literal 'Z's such a name are doubled
--   to 'ZZ'.
--
sanitizeName :: String -> String
sanitizeName str
 = concatMap rewriteChar str


-- | Get the encoded version of a character.
rewriteChar :: Char -> String
rewriteChar c
 = fromMaybe [c] $ convertSymbol c


-- | Convert symbols to their sanitized form.
convertSymbol :: Char -> Maybe String
convertSymbol c
 = case c of
        'Z'     -> Just "ZZ"
        '!'     -> Just "Zba"
        '@'     -> Just "Zat"
        '#'     -> Just "Zha"
        '$'     -> Just "Zdl"
        '%'     -> Just "Zpl"
        '^'     -> Just "Zha"
        '&'     -> Just "Zam"
        '*'     -> Just "Zas"
        '~'     -> Just "Ztl"
        '-'     -> Just "Zmi"
        '+'     -> Just "Zpl"
        '='     -> Just "Zeq"
        '|'     -> Just "Zpi"
        '\\'    -> Just "Zbs"
        '/'     -> Just "Zfs"
        ':'     -> Just "Zco"
        '?'     -> Just "Zqn"
        '<'     -> Just "Zlt"
        '>'     -> Just "Zgt"
        '['     -> Just "Zbr"
        ']'     -> Just "Zke"
        '\''    -> Just "Zpm"
        '`'     -> Just "Zbt"
        _       -> Nothing
