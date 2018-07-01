
module DDC.Core.Exp.DaCon
        ( DaCon (..)
        , DaConBoundName (..)

        -- * Compounds
        , dcUnit
        , takeNameOfDaConPrim
        , takeNameOfDaConBound
        , takeBaseCtorNameOfDaCon)
where
import DDC.Core.Module.Name
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
        | DaConPrim     n

        -- | Data constructor that has a data type declaration.
        --   These can be user-defined data constructors, as well as
        --   primitive algebraic data constructors.
        | DaConBound    (DaConBoundName n)
        deriving (Show, Eq)


-- | Bound occurrence of data constructor that is associated with a data type
--   declaration. This includes user defined data types, as well as primitive
--   algebraic data types like 'Bool'. Once we have noticed a data constructor
--   is primitive it will be rewritten to a 'DaConPrim' by the type checker.
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
        DaConPrim  n    -> rnf n
        DaConBound n    -> rnf n


-------------------------------------------------------------------------------
-- | Take the name of a primitive data constructor, if there is one.
takeNameOfDaConPrim :: DaCon n t -> Maybe n
takeNameOfDaConPrim dc
 = case dc of
        DaConPrim n     -> Just n
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
        DaConPrim n     -> Just n
        _               -> Nothing


-- | The unit data constructor.
dcUnit  :: DaCon n t
dcUnit  = DaConUnit

