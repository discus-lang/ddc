
module DDC.Core.Exp.DaCon
        ( DaCon (..)
        , DaConBoundName (..)

        -- * Compounds
        , dcUnit
        , takeNameOfDaConPrim
        , takeNameOfDaConBound
        , takeBaseCtorNameOfDaCon
        , takeTypeOfDaCon)
where
import DDC.Core.Module.Name
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
        --   These can be user-defined data constructors, as well as
        --   primitive algebraic data constructors.
        | DaConBound    (DaConBoundName n)
        deriving (Show, Eq)


-- | Bound occurrence of data constructor that is associated with a data type
--   declaration. This includes user defined data types, as well as primitive
--   algebraic data types like 'Tuple'.
--
--   The module and enclosing type names are optional as we also use this
--   representation when they have not been resolved yet.
--
data DaConBoundName n
        = DaConBoundName
        { daConBoundNameModule  :: !(Maybe ModuleName)
        , daConBoundTypeName    :: !(Maybe n)
        , daConBoundNameCtor    :: n }
        deriving (Show, Eq, Ord)


instance NFData n => NFData (DaConBoundName n) where
 rnf (DaConBoundName mm mt nc)
        = rnf mm `seq` rnf mt `seq` rnf nc


instance (NFData n, NFData t) => NFData (DaCon n t) where
 rnf !dc
  = case dc of
        DaConUnit       -> ()
        DaConRecord ns  -> rnf ns
        DaConPrim  n t  -> rnf n  `seq` rnf t
        DaConBound n    -> rnf n


-------------------------------------------------------------------------------
-- | Take the name of a primitive data constructor, if there is one.
takeNameOfDaConPrim :: DaCon n t -> Maybe n
takeNameOfDaConPrim dc
 = case dc of
        DaConPrim n _   -> Just n
        _               -> Nothing


-- | Take the name of a bound data constructor, if this is one.
takeNameOfDaConBound :: DaCon n t -> Maybe (DaConBoundName n)
takeNameOfDaConBound dc
 = case dc of
        DaConBound n    -> Just n
        _               -> Nothing


-- | Get the base constructor name of a DaCon,
--   ignoring the module ane type name fields.
takeBaseCtorNameOfDaCon :: DaCon n t -> Maybe n
takeBaseCtorNameOfDaCon dc
 = case dc of
        DaConBound (DaConBoundName _ _ n)
                        -> Just n
        DaConPrim n _   -> Just n
        _               ->  Nothing


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

