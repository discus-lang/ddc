
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

          -- * Primitive type constructors.
        , PrimTyCon     (..)
        , readPrimTyCon
        , kindPrimTyCon

          -- * Primitive arithmetic operators.
        , PrimArith     (..)
        , readPrimArith
        , typePrimArith

          -- * Primitive numeric casts.
        , PrimCast      (..)
        , readPrimCast
        , typePrimCast)
where
import DDC.Core.Tetra.Prim.Base
import DDC.Core.Tetra.Prim.TyConTetra
import DDC.Core.Tetra.Prim.TyConPrim
import DDC.Core.Tetra.Prim.DaConTetra
import DDC.Core.Tetra.Prim.OpArith
import DDC.Core.Tetra.Prim.OpCast
import DDC.Core.Tetra.Prim.OpFun
import DDC.Data.ListUtils
import DDC.Type.Exp
import DDC.Base.Pretty
import DDC.Base.Name
import Control.DeepSeq
import Data.Char        
import Data.List
import qualified Data.ByteString        as BS
import qualified Data.Vector            as V

import DDC.Core.Lexer.Names             (isVarStart)
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

        NameOpFun      op       -> rnf op

        NamePrimTyCon  op       -> rnf op
        NamePrimArith  op       -> rnf op
        NamePrimCast   op       -> rnf op

        NameLitBool b           -> rnf b
        NameLitNat  n           -> rnf n
        NameLitInt  i           -> rnf i
        NameLitSize    s        -> rnf s
        NameLitWord i bits      -> rnf i `seq` rnf bits
        NameLitFloat   d bits   -> rnf d `seq` rnf bits
        NameLitArray   vec      -> rnf vec
        NameLitString  bs       -> rnf bs       

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
        
        NameOpFun      op       -> ppr op

        NamePrimTyCon  op       -> ppr op
        NamePrimArith  op       -> ppr op
        NamePrimCast   op       -> ppr op

        NameLitBool True        -> text "True#"
        NameLitBool False       -> text "False#"
        NameLitNat  i           -> integer i <> text "#"
        NameLitInt  i           -> integer i <> text "i" <> text "#"
        NameLitSize    s        -> integer s <> text "s"
        NameLitWord i bits      -> integer i <> text "w" <> int bits <> text "#"
        NameLitFloat   f bits   -> double  f <> text "f" <> int bits

        NameLitArray   vec      
         -> text "[#" 
         <> hcat (punctuate (text ",") (map ppr $ V.toList vec)) 
         <> text "#]"

        NameLitString  bs       
         -> text (show $ BS.unpack bs)

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

        | Just p <- readOpFun     str
        = Just $ NameOpFun p

        -- Primitive names.
        | Just p <- readPrimTyCon str  
        = Just $ NamePrimTyCon p

        | Just p <- readPrimArith str  
        = Just $ NamePrimArith p

        | Just p <- readPrimCast  str
        = Just $ NamePrimCast  p

        -- Literal Bools
        | str == "True#"  = Just $ NameLitBool True
        | str == "False#" = Just $ NameLitBool False

        -- Literal Nat
        | Just str'     <- stripSuffix "#" str
        , Just val      <- readLitNat str'
        = Just $ NameLitNat  val

        -- Literal Ints
        | Just str'     <- stripSuffix "#" str
        , Just val      <- readLitInt str'
        = Just $ NameLitInt  val

        -- Literal Words
        | Just str'     <- stripSuffix "#" str
        , Just (val, bits) <- readLitWordOfBits str'
        , elem bits [8, 16, 32, 64]
        = Just $ NameLitWord val bits

        -- Unboxed literals.
        | Just base        <- stripPrefix "##" (reverse str)
        , Just n           <- readName (reverse base ++ "#")
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
        NameLitWord _ bits      -> Just (tWord bits)
        _                       -> Nothing


-- | Take the type of a primitive operator.
takeTypeOfPrimOpName :: Name -> Maybe (Type Name)
takeTypeOfPrimOpName nn
 = case nn of
        NameOpFun       op -> Just (typeOpFun     op)
        NamePrimArith   op -> Just (typePrimArith op)
        NamePrimCast    op -> Just (typePrimCast  op)
        _                  -> Nothing

