
module DDC.Core.Tetra.Prim
        ( -- * Names and lexing.
          Name          (..)
        , isNameHole
        , isNameLit
        , isNameLitUnboxed
        , readName
        , takeTypeOfLitName
        , takeTypeOfPrimOpName

          -- * Baked-in type constructors.
        , TyConTetra     (..)
        , readTyConTetra
        , kindTyConTetra
        , tTupleN, tUnboxed, tFunValue, tCloValue, tTextLit

          -- * Baked-in data constructors.
        , DaConTetra     (..)
        , readDaConTetra
        , typeDaConTetra
        , xTuple2
        , dcTuple2
        , dcTupleN 

          -- * Baked-in function operators.
        , OpFun         (..)
        , readOpFun
        , typeOpFun

          -- * Baked-in vector operators.
        , OpVector      (..)
        , readOpVectorFlag
        , typeOpVectorFlag

          --- * Baked-in error handling.
        , OpError       (..)
        , readOpErrorFlag
        , typeOpErrorFlag

          -- * Primitive type constructors.
        , PrimTyCon     (..)
        , pprPrimTyConStem
        , readPrimTyCon,        readPrimTyConStem
        , kindPrimTyCon

          -- * Primitive arithmetic operators.
        , PrimArith     (..)
        , readPrimArithFlag
        , typePrimArithFlag

          -- * Primitive numeric casts.
        , PrimCast      (..)
        , readPrimCast
        , typePrimCast)
where
import DDC.Core.Tetra.Prim.Base
import DDC.Core.Tetra.Prim.TyConTetra
import DDC.Core.Tetra.Prim.TyConPrim
import DDC.Core.Tetra.Prim.DaConTetra
import DDC.Core.Tetra.Prim.OpError
import DDC.Core.Tetra.Prim.OpArith
import DDC.Core.Tetra.Prim.OpCast
import DDC.Core.Tetra.Prim.OpFun
import DDC.Core.Tetra.Prim.OpVector
import DDC.Data.ListUtils
import DDC.Type.Exp
import DDC.Base.Pretty
import DDC.Base.Name
import Control.DeepSeq
import Data.Char        
import qualified Data.Text              as T

import DDC.Core.Lexer.Token.Names       (isVarStart)
import DDC.Core.Salt.Name 
        ( readLitNat
        , readLitInt
        , readLitWordOfBits)

instance NFData Name where
 rnf nn
  = case nn of
        NameVar s               -> rnf s
        NameCon s               -> rnf s
        NameExt n s             -> rnf n `seq` rnf s
        
        NameTyConTetra con      -> rnf con
        NameDaConTetra con      -> rnf con

        NameOpError    op !_    -> rnf op
        NameOpFun      op       -> rnf op
        NameOpVector   op !_    -> rnf op

        NamePrimTyCon  op       -> rnf op
        NamePrimArith  op !_    -> rnf op
        NamePrimCast   op       -> rnf op

        NameLitBool    b        -> rnf b
        NameLitNat     n        -> rnf n
        NameLitInt     i        -> rnf i
        NameLitSize    s        -> rnf s
        NameLitWord    i bits   -> rnf i `seq` rnf bits
        NameLitFloat   d bits   -> rnf d `seq` rnf bits
        NameLitTextLit bs       -> rnf bs       

        NameLitUnboxed n        -> rnf n

        NameHole                -> ()


instance Pretty Name where
 ppr nn
  = case nn of
        NameVar  v              -> text v
        NameCon  c              -> text c
        NameExt  n s            -> ppr n <> text "$" <> text s

        NameTyConTetra tc       -> ppr tc
        NameDaConTetra dc       -> ppr dc
        
        NameOpError    op False -> ppr op
        NameOpError    op True  -> ppr op <> text "#"


        NameOpFun      op       -> ppr op

        NameOpVector   op False -> ppr op
        NameOpVector   op True  -> ppr op <> text "#"

        NamePrimTyCon  op       -> ppr op

        NamePrimArith  op False -> ppr op
        NamePrimArith  op True  -> ppr op <> text "#"

        NamePrimCast   op       -> ppr op

        NameLitBool True        -> text "True#"
        NameLitBool False       -> text "False#"
        NameLitNat  i           -> integer i             <> text "#"
        NameLitInt  i           -> integer i <> text "i" <> text "#"
        NameLitSize    s        -> integer s <> text "s" <> text "#"
        NameLitWord    i bits   -> integer i <> text "w" <> int bits <> text "#"
        NameLitFloat   f bits   -> double  f <> text "f" <> int bits <> text "#"
        NameLitTextLit tx       -> text (show $ T.unpack tx)         <> text "#"

        NameLitUnboxed n        -> ppr n <> text "#"

        NameHole                -> text "?"


instance CompoundName Name where
 extendName n str       
  = NameExt n str
 
 splitName nn
  = case nn of
        NameExt n str   -> Just (n, str)
        _                -> Nothing


-- | Read the name of a variable, constructor or literal.
readName :: String -> Maybe Name
readName str
        -- Baked-in names.
        | Just p <- readTyConTetra str
        = Just $ NameTyConTetra p

        | Just p <- readDaConTetra str
        = Just $ NameDaConTetra p

        | Just (p,f) <- readOpErrorFlag   str
        = Just $ NameOpError p f

        | Just p <- readOpFun     str
        = Just $ NameOpFun p

        | Just (p, f) <- readOpVectorFlag  str
        = Just $ NameOpVector p f

        -- Primitive names.
        | Just p <- readPrimTyCon str  
        = Just $ NamePrimTyCon p

        | Just (p, f) <- readPrimArithFlag str  
        = Just $ NamePrimArith p f

        | Just p <- readPrimCast  str
        = Just $ NamePrimCast  p

        -- Literal Bools
        | str == "True"  = Just $ NameLitBool True
        | str == "False" = Just $ NameLitBool False

        -- Literal Nat
        | Just val      <- readLitNat str
        = Just $ NameLitNat  val

        -- Literal Ints
        | Just val      <- readLitInt str
        = Just $ NameLitInt  val

        -- Literal Words
        | Just (val, bits) <- readLitWordOfBits str
        , elem bits [8, 16, 32, 64]
        = Just $ NameLitWord val bits

        -- Unboxed literals.
        | Just base        <- stripSuffix "#" str
        , Just n           <- readName base
        = case n of
                NameLitBool{}   -> Just n
                NameLitNat{}    -> Just n
                NameLitInt{}    -> Just n
                NameLitWord{}   -> Just n
                _               -> Nothing

        -- Holes
        | str == "?"
        = Just $ NameHole

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


-- | Get the type associated with a literal name.
takeTypeOfLitName :: Name -> Maybe (Type Name)
takeTypeOfLitName nn
 = case nn of
        NameLitBool{}           -> Just tBool
        NameLitNat{}            -> Just tNat
        NameLitInt{}            -> Just tInt
        NameLitWord _ bits      -> Just (tWord  bits)
        NameLitFloat _ bits     -> Just (tFloat bits)
        NameLitTextLit _        -> Just tTextLit
        _                       -> Nothing


-- | Take the type of a primitive operator.
takeTypeOfPrimOpName :: Name -> Maybe (Type Name)
takeTypeOfPrimOpName nn
 = case nn of
        NameOpError     op f    -> Just (typeOpErrorFlag   op f)
        NameOpFun       op      -> Just (typeOpFun         op)
        NameOpVector    op f    -> Just (typeOpVectorFlag  op f)
        NamePrimArith   op f    -> Just (typePrimArithFlag op f)
        NamePrimCast    op      -> Just (typePrimCast      op)
        _                       -> Nothing

