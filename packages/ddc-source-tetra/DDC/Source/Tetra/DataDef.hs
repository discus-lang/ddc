{-# LANGUAGE UndecidableInstances #-}

-- | Source Tetra data type definitions.
module DDC.Source.Tetra.DataDef
        ( -- * Data Type Definition.
          DataDef  (..)
--        , typeEnvOfDataDef
          
          -- * Data Constructor Definition.
        , DataCtor (..)
        , typeOfDataCtor)
where
import DDC.Source.Tetra.Exp.Generic
-- import DDC.Type.Env             (TypeEnv)
-- import qualified DDC.Type.Env   as Env
import Control.DeepSeq


-- DataDef --------------------------------------------------------------------
-- | Data type definitions.
data DataDef l
        = DataDef
        { -- | Data type name.
          dataDefTypeName       :: !(GTBindCon l)

          -- | Type parameters and their kinds.
        , dataDefParams         :: [(GTBindVar l, GType l)]

          -- | Parameters and return type of each constructor.
        , dataDefCtors          :: [DataCtor l] }

deriving instance (ShowLanguage l, Show (DataCtor l))
 => Show (DataDef l)

instance NFData (DataDef n) where
 rnf !_ = ()


-- | Take the types of data constructors from a data type definition.
{-
typeEnvOfDataDef :: Ord n => DataDef n -> TypeEnv n
typeEnvOfDataDef def 
 = Env.fromList 
        [BName  (dataCtorName ctor) 
                (typeOfDataCtor def ctor)
                | ctor  <- dataDefCtors def ]
-}              

-- DataCtor -------------------------------------------------------------------
-- | A data type constructor definition.
data DataCtor l
        = DataCtor
        { -- | Name of the data constructor.
          dataCtorName          :: !(GXBindCon l)

          -- | Types of each of the fields of the constructor.
        , dataCtorFieldTypes    :: ![GType l]

          -- | Result type of the constructor.
        , dataCtorResultType    :: !(GType l) }

deriving instance (ShowLanguage l) 
 => Show (DataCtor l)



instance NFData (DataCtor n) where
 rnf !_ = ()


-- | Get the type of a data constructor.
typeOfDataCtor :: DataDef l -> DataCtor l -> GType l
typeOfDataCtor def ctor
        = foldr (\(b, k) -> TForall k b)
                (foldr TFun (dataCtorResultType ctor)
                            (dataCtorFieldTypes ctor))
                (dataDefParams def)
