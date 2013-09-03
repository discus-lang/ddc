
module DDC.Core.Exp.DaCon 
        ( DaCon (..)

        -- * Compounds
        , dcUnit
        , takeNameOfDaCon
        , takeTypeOfDaCon)
where
import DDC.Type.Compounds
import DDC.Type.Exp
import Control.DeepSeq


-- | Data constructors.
--
--   Algebraic constructors can be deconstructed with case-expressions,
--   and must have a data type declaration. Non-algebraic types like 'Float'
--   can't be inspected with case-expressions.
--   TODO: shift daConIsAlgebraic into data type definition.
--
data DaCon n
        -- | Baked in unit data constructor.
        = DaConUnit

        -- | Primitive data constructor that has its type attached.
        --   Used for literals and other baked-in data constructors.
        | DaConPrim
        { -- | Name of the data constructor.
          daConName             :: !n 

          -- | Type of the data constructor.
        , daConType             :: !(Type n)

          -- | Whether the data type is algebraic.      -- TODO: remove this.
        , daConIsAlgebraic      :: Bool 
        }

        -- | Algebraic data constructor that has a data type declaration.
        | DaConBound
        { -- | Name of the data constructor.
          daConName             :: !n
        }
        deriving (Show, Eq)


instance NFData n => NFData (DaCon n) where
 rnf !dc
  = case dc of
        DaConUnit               -> ()
        DaConPrim  n t f        -> rnf n `seq` rnf t `seq` rnf f
        DaConBound n            -> rnf n


-- | Take the name of data constructor,
--   if there is one.
takeNameOfDaCon :: DaCon n -> Maybe n
takeNameOfDaCon dc
 = case dc of
        DaConUnit               -> Nothing
        DaConPrim{}             -> Just $ daConName dc
        DaConBound{}            -> Just $ daConName dc


-- | Take the type annotation of a data constructor,
--   if we know it locally.
takeTypeOfDaCon :: DaCon n -> Maybe (Type n)
takeTypeOfDaCon dc  
 = case dc of
        DaConUnit               -> Just $ tUnit
        DaConPrim{}             -> Just $ daConType dc
        DaConBound{}            -> Nothing


-- | The unit data constructor.
dcUnit  :: DaCon n
dcUnit  = DaConUnit

