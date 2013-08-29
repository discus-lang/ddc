
module DDC.Core.Exp.DaCon 
        ( DaCon         (..)
        , DaConName     (..)

        -- * Compounds
        , dcUnit
        , mkDaConAlg
        , mkDaConSolid
        , takeNameOfDaCon
        , typeOfDaCon)
where
import DDC.Type.Compounds
import DDC.Type.Exp
import Control.DeepSeq


-------------------------------------------------------------------------------
-- | Data constructor names.
data DaConName n
        -- | The unit data constructor is builtin.
        = DaConUnit

        -- | Data constructor name defined by the client.
        | DaConNamed n
        deriving (Eq, Show)


instance NFData n => NFData (DaConName n) where
 rnf dcn
  = case dcn of
        DaConUnit       -> ()
        DaConNamed n    -> rnf n


-------------------------------------------------------------------------------
-- | Data constructors.
--   TODO: make DaConLit, DaConBound
--   TODO: IsAlgebraic flag should be stored in the data type declaration.
data DaCon n
        = DaCon
        { -- | Name of the data constructor.
          daConName             :: !(DaConName n)

          -- | Type of the data constructor.
          --   The type must be closed.
        , daConType             :: !(Type n)

          -- | Algebraic constructors can be deconstructed with case-expressions,
          --   and must have a data type declaration.
          -- 
          --   Non-algebraic types like 'Float' can't be inspected with
          --   case-expressions.
        , daConIsAlgebraic      :: !Bool }
        deriving (Show, Eq)


instance NFData n => NFData (DaCon n) where
 rnf !dc
        =     rnf (daConName dc)
        `seq` rnf (daConType dc)
        `seq` rnf (daConIsAlgebraic dc)


-- | Take the name of data constructor.
takeNameOfDaCon :: DaCon n -> Maybe n
takeNameOfDaCon dc
 = case daConName dc of
        DaConUnit               -> Nothing
        DaConNamed n            -> Just n


-- | Take the type annotation of a data constructor.
typeOfDaCon :: DaCon n -> Type n
typeOfDaCon dc  = daConType dc


-- | The unit data constructor.
dcUnit  :: DaCon n
dcUnit  = DaCon
        { daConName             = DaConUnit
        , daConType             = tUnit
        , daConIsAlgebraic      = True }


-- | Make an algebraic data constructor.
mkDaConAlg :: n -> Type n -> DaCon n
mkDaConAlg n t
        = DaCon
        { daConName             = DaConNamed n
        , daConType             = t
        , daConIsAlgebraic      = True }


-- | Make a non-algebraic (solid) constructor.
--   These are used for location values in the interpreter,
--   and for floating point literals in the main compiler.
mkDaConSolid :: n -> Type n -> DaCon n
mkDaConSolid n t
        = DaCon
        { daConName             = DaConNamed n
        , daConType             = t
        , daConIsAlgebraic      = False }
