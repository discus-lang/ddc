
module DDC.Source.Tetra.Prim.Base
        ( Name          (..)
        , TyConData     (..), readTyConData
        , OpStore       (..), readOpStore
        , PrimTyCon     (..), readPrimTyCon
        , PrimArith     (..), readPrimArith)
where
import Control.DeepSeq
import DDC.Core.Tetra    
        ( TyConData     (..), readTyConData
        , OpStore       (..), readOpStore
        , PrimTyCon     (..), readPrimTyCon
        , PrimArith     (..), readPrimArith)


-- | Names of things used in Disciple Source Tetra.
data Name
        -- | A user defined variable.
        = NameVar               String

        -- | A user defined constructor.
        | NameCon               String

        -- Baked in things ----------------------
        -- | Baked in data type constructors.
        | NameTyConData         TyConData

        -- | Baked in store operators.
        | NameOpStore           OpStore

        -- Machine primitives -------------------
        -- | Primitive type cosntructors.
        | NamePrimTyCon         PrimTyCon

        -- | Primitive arithmetic, logic and comparison.
        | NamePrimArith         PrimArith

        -- Literals -----------------------------
        -- | A boolean literal.
        | NameLitBool           Bool

        -- | A natural literal.
        | NameLitNat            Integer

        -- | An integer literal.
        | NameLitInt            Integer

        -- | A word literal.
        | NameLitWord           Integer Int

        -- Inference ----------------------------
        -- | A hole used during type inference.
        | NameHole              
        deriving (Eq, Ord, Show)


instance NFData Name where
 rnf nn
  = case nn of
        NameVar s               -> rnf s
        NameCon s               -> rnf s

        NameTyConData p         -> rnf p
        NameOpStore   p         -> rnf p
        NamePrimTyCon p         -> rnf p
        NamePrimArith p         -> rnf p

        NameLitBool b           -> rnf b
        NameLitNat  n           -> rnf n
        NameLitInt  i           -> rnf i
        NameLitWord i bits      -> rnf i `seq` rnf bits

        NameHole                -> ()

