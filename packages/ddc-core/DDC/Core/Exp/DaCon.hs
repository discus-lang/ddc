
module DDC.Core.Exp.DaCon 
        ( DaCon (..)

        -- * Compounds
        , dcUnit
        , takeNameOfDaCon
        , takeTypeOfDaCon)
where
import DDC.Type.Exp.Simple
import Control.DeepSeq
import Data.Text                (Text)

-------------------------------------------------------------------------------
-- | Data constructors.
data DaCon n t
        -- | Baked in unit data constructor.
        = DaConUnit

        -- | Baked in record data constructor,
        --   with the given field names.
        | DaConRecord   [Text]

        -- | Primitive data constructor used for literals and baked-in
        --   constructors. 
        --
        --   The type of the constructor needs to be attached to handle the
        --   case where there are too many constructors in the data type to
        --   list, like for Int literals. In this case we determine what data
        --   type it belongs to from the attached type of the data constructor.
        --   
        | DaConPrim
        { -- | Name of the data constructor.
          daConName     :: !n 

          -- | Type of the data constructor.
        , daConType     :: !t
        }

        -- | Data constructor that has a data type declaration.
        | DaConBound
        { -- | Name of the data constructor.
          daConName     :: !n
        }
        deriving (Show, Eq)


instance (NFData n, NFData t) => NFData (DaCon n t) where
 rnf !dc
  = case dc of
        DaConUnit       -> ()
        DaConRecord ns  -> rnf ns 
        DaConPrim  n t  -> rnf n  `seq` rnf t
        DaConBound n    -> rnf n


-------------------------------------------------------------------------------
-- | Take the name of data constructor,
--   if there is one.
takeNameOfDaCon :: DaCon n t -> Maybe n
takeNameOfDaCon dc
 = case dc of
        DaConUnit       -> Nothing
        DaConRecord{}   -> Nothing
        DaConPrim{}     -> Just $ daConName dc
        DaConBound{}    -> Just $ daConName dc


-- | Take the type annotation of a data constructor,
--   if we know it locally.
takeTypeOfDaCon :: DaCon n (Type n) -> Maybe (Type n)
takeTypeOfDaCon dc  
 = case dc of
        DaConUnit       -> Just $ tUnit

        DaConRecord ns  
         -> Just $  tForalls (map (const kData) ns)
                 $  \tsArg -> tFunOfParamResult tsArg
                           $  tApps (TCon (TyConSpec (TcConRecord ns))) tsArg

        DaConPrim{}     -> Just $ daConType dc
        DaConBound{}    -> Nothing


-- | The unit data constructor.
dcUnit  :: DaCon n t
dcUnit  = DaConUnit

