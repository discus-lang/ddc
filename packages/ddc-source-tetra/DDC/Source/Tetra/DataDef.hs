{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

-- | Source Tetra data type definitions.
module DDC.Source.Tetra.DataDef
        ( -- * Data Type Definition.
          DataDef  (..)
        , envOfDataDef
          
          -- * Data Constructor Definition.
        , DataCtor (..)
        , typeOfDataCtor)
where
import DDC.Source.Tetra.Exp.Generic
import DDC.Source.Tetra.Exp.Source
import DDC.Source.Tetra.Env             (Env)
import qualified DDC.Source.Tetra.Env   as Env
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
envOfDataDef 
        :: DataDef Source -> Env

envOfDataDef def 
        =  Env.unions
        $ [Env.singletonDaCon (dataCtorName ctor) (typeOfDataCtor def ctor)
                | ctor  <- dataDefCtors def]
              

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
        = foldr (\(b, k) t -> TApp (TCon (TyConForall k)) (TAbs b k t))
                (foldr TFun (dataCtorResultType ctor)
                            (dataCtorFieldTypes ctor))
                (dataDefParams def)




