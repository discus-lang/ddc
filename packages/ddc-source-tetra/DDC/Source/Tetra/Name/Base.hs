
module DDC.Source.Tetra.Name.Base
        ( Name          (..)
        , TyConPrim     (..), readTyConPrim
        , PrimArith     (..), readPrimArith
        , PrimRef       (..), readPrimRef)
where
import Control.DeepSeq
import DDC.Core.Tetra    
        ( TyConPrim     (..), readTyConPrim
        , PrimArith     (..), readPrimArith
        , PrimRef       (..), readPrimRef)


-- | Names of things used in Disciple Source Tetra.
data Name
        -- | A user defined variable.
        = NameVar               String

        -- | A user defined constructor.
        | NameCon               String

        -- Machine primitives -------------------
        -- | Primitive type cosntructors.
        | NameTyConPrim         TyConPrim        

        -- | Primitive arithmetic, logic and comparison.
        | NamePrimArith         PrimArith

        -- | Mutable references.
        | NamePrimRef           PrimRef


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

        NameTyConPrim con       -> rnf con
        NamePrimArith con       -> rnf con
        NamePrimRef   con       -> rnf con

        NameLitBool b           -> rnf b
        NameLitNat  n           -> rnf n
        NameLitInt  i           -> rnf i
        NameLitWord i bits      -> rnf i `seq` rnf bits

        NameHole                -> ()

