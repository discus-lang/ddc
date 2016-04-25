
-- | Definitions of Source Tetra primitive names and operators.
module DDC.Source.Tetra.Prim
        ( -- * Names
          Name          (..)

          -- * Primitive Names
        , PrimName      (..)
        , pattern NameType
        , pattern NameVal
        , readName

          -- * Primitive Types
        , PrimType      (..)
        , pattern NameTyCon
        , pattern NameTyConTetra

          -- ** Primitive machine type constructors.
        , PrimTyCon     (..)
        , kindPrimTyCon
        , tBool
        , tNat
        , tInt
        , tSize
        , tWord
        , tFloat
        , tTextLit

          -- ** Primitive tetra type constructors.
        , PrimTyConTetra(..)
        , pattern NameTyConTetraTuple
        , pattern NameTyConTetraF
        , pattern NameTyConTetraC
        , pattern NameTyConTetraU
        , kindPrimTyConTetra

          -- * Primitive values
        , PrimVal (..)
        , pattern NameLit
        , pattern NameArith
        , pattern NameVector
        , pattern NameFun
        , pattern NameError

          -- ** Primitive arithmetic operators.
        , PrimArith     (..)
        , typePrimArith

          -- ** Primitive vector operators.
        , OpVector      (..)
        , typeOpVector

          -- ** Primitive function operators.
        , OpFun         (..)
        , typeOpFun

          -- ** Primitive error handling
        , OpError (..)
        , typeOpError

          -- ** Primitive literals
        , PrimLit (..)
        , pattern NameLitBool
        , pattern NameLitNat
        , pattern NameLitInt
        , pattern NameLitSize
        , pattern NameLitWord
        , pattern NameLitFloat
        , pattern NameLitTextLit)
where
import DDC.Source.Tetra.Prim.Base
import DDC.Source.Tetra.Prim.TyConPrim
import DDC.Source.Tetra.Prim.TyConTetra
import DDC.Source.Tetra.Prim.OpArith
import DDC.Source.Tetra.Prim.OpFun
import DDC.Source.Tetra.Prim.OpVector
import DDC.Source.Tetra.Prim.OpError
import DDC.Core.Lexer.Names             (isVarStart)
import DDC.Base.Pretty
import Control.DeepSeq
import Data.Char
import qualified Data.Text              as T

import DDC.Core.Tetra   
        ( readPrimTyCon
        , readPrimArithFlag
        , readOpFun
        , readOpError
        , readOpVectorFlag)

import DDC.Core.Salt.Name
        ( readLitNat
        , readLitInt
        , readLitSize
        , readLitWordOfBits
        , readLitFloatOfBits)


---------------------------------------------------------------------------------------------------
instance Pretty Name where
 ppr nn
  = case nn of
        NameVar  v              -> text v
        NameCon  c              -> text c
        NamePrim p              -> ppr p
        NameHole                -> text "?"


instance NFData Name where
 rnf nn
  = case nn of
        NameVar s               -> rnf s
        NameCon s               -> rnf s
        NamePrim p              -> rnf p
        NameHole                -> ()


-- | Read the name of a variable, constructor or literal.
readName :: String -> Maybe Name
readName str
        -- Primitive names.
        | Just n        <- readPrimName str
        = Just $ NamePrim n

        -- Constructors.
        | c : _         <- str
        , isUpper c
        = Just $ NameCon str

        -- Variables.
        | c : _         <- str
        , isVarStart c      
        = Just $ NameVar str

        | otherwise
        = Nothing


---------------------------------------------------------------------------------------------------
instance Pretty PrimName where
 ppr nn
  = case nn of
        PrimNameType p          -> ppr p
        PrimNameVal p           -> ppr p


instance NFData PrimName where
 rnf nn
  = case nn of
        PrimNameType p          -> rnf p
        PrimNameVal p           -> rnf p


readPrimName :: String -> Maybe PrimName
readPrimName str
        | Just t <- readPrimType str
        = Just $ PrimNameType t

        | Just v <- readPrimVal str
        = Just $ PrimNameVal  v

        | otherwise
        = Nothing


---------------------------------------------------------------------------------------------------
instance Pretty PrimType where
 ppr t
  = case t of
        PrimTypeTyConTetra p    -> ppr p
        PrimTypeTyCon  p        -> ppr p


instance NFData PrimType where
 rnf t
  = case t of
        PrimTypeTyConTetra p    -> rnf p
        PrimTypeTyCon p         -> rnf p


-- | Read the name of a primitive type.
readPrimType :: String -> Maybe PrimType
readPrimType str
        | Just p <- readPrimTyConTetra str  
        = Just $ PrimTypeTyConTetra p

        | Just p <- readPrimTyCon str  
        = Just $ PrimTypeTyCon p

        | otherwise
        = Nothing


---------------------------------------------------------------------------------------------------
instance Pretty PrimVal where
 ppr val
  = case val of
        PrimValError  p         -> ppr p
        PrimValLit    lit       -> ppr lit
        PrimValArith  p         -> ppr p
        PrimValVector p         -> ppr p
        PrimValFun    p         -> ppr p


instance NFData PrimVal where
 rnf val
  = case val of
        PrimValError  p         -> rnf p
        PrimValLit    lit       -> rnf lit
        PrimValArith  p         -> rnf p
        PrimValVector p         -> rnf p
        PrimValFun    p         -> rnf p


-- | Read the name of a primtive value.
readPrimVal :: String -> Maybe PrimVal
readPrimVal str
        | Just p          <- readOpError str
        = Just $ PrimValError  p

        | Just lit        <- readPrimLit str
        = Just $ PrimValLit    lit

        | Just (p, False) <- readPrimArithFlag str  
        = Just $ PrimValArith  p

        | Just (p, False) <- readOpVectorFlag str
        = Just $ PrimValVector p

        | Just p          <- readOpFun str
        = Just $ PrimValFun    p

        | otherwise
        = Nothing


---------------------------------------------------------------------------------------------------
instance Pretty PrimLit where
 ppr lit
  = case lit of
        PrimLitBool    True     -> text "True"
        PrimLitBool    False    -> text "False"
        PrimLitNat     i        -> integer i
        PrimLitInt     i        -> integer i <> text "i"
        PrimLitSize    s        -> integer s <> text "s"
        PrimLitWord    i bits   -> integer i <> text "w" <> int bits
        PrimLitFloat   f bits   -> double  f <> text "f" <> int bits
        PrimLitTextLit tx       -> text (show $ T.unpack tx)


instance NFData PrimLit where
 rnf lit 
  = case lit of
        PrimLitBool    b        -> rnf b
        PrimLitNat     n        -> rnf n
        PrimLitInt     i        -> rnf i
        PrimLitSize    s        -> rnf s
        PrimLitWord    i bits   -> rnf i `seq` rnf bits
        PrimLitFloat   d bits   -> rnf d `seq` rnf bits
        PrimLitTextLit bs       -> rnf bs       


-- | Read the name of a primitive literal.
readPrimLit :: String -> Maybe PrimLit
readPrimLit str
        -- Literal Bools
        | str == "True"        = Just $ PrimLitBool True
        | str == "False"       = Just $ PrimLitBool False

        -- Literal Nat
        | Just val <- readLitNat str
        = Just $ PrimLitNat  val

        -- Literal Ints
        | Just val <- readLitInt str
        = Just $ PrimLitInt  val

        -- Literal Sizes
        | Just val <- readLitSize str
        = Just $ PrimLitSize val

        -- Literal Words
        | Just (val, bits) <- readLitWordOfBits str
        , elem bits [8, 16, 32, 64]
        = Just $ PrimLitWord val bits

        -- Literal Floats
        | Just (val, bits) <- readLitFloatOfBits str
        , elem bits [32, 64]
        = Just $ PrimLitFloat val bits

        | otherwise
        = Nothing

