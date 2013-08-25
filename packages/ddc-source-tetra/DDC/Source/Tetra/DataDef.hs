
module DDC.Source.Tetra.DataDef
        ( -- * Data Type Definition.
          DataDef  (..)
        , typeEnvOfDataDef
          
          -- * Data Constructor Definition.
        , DataCtor (..)
        , typeOfDataCtor)
where
import DDC.Type.Compounds
import DDC.Type.Exp
import DDC.Type.Env             (TypeEnv)
import qualified DDC.Type.Env   as Env
import Control.DeepSeq


-- DataDef --------------------------------------------------------------------
-- | Data type definitions.
data DataDef n
        = DataDef
        { -- | Data type name.
          dataDefTypeName       :: !n

          -- | Type parameters.
        , dataDefParams         :: [Bind n]

          -- | Parameters and return type of each constructor.
        , dataDefCtors          :: [DataCtor n] }
        deriving Show

instance NFData (DataDef n)


-- | Take the types of data constructors from a data type definition.
typeEnvOfDataDef :: Ord n => DataDef n -> TypeEnv n
typeEnvOfDataDef def 
 = Env.fromList 
        [BName  (dataCtorName ctor) 
                (typeOfDataCtor def ctor)
                | ctor  <- dataDefCtors def ]
                

-- DataCtor -------------------------------------------------------------------
-- | A data type constructor definition.
data DataCtor n
        = DataCtor
        { -- | Name of the data constructor.
          dataCtorName          :: !n

          -- | Types of each of the fields of the constructor.
        , dataCtorFieldTypes    :: ![Type n]

          -- | Result type of the constructor.
          --   TODO: Check that this is the same as the type being defined.
        , dataCtorResultType    :: !(Type n) }
        deriving Show


instance NFData (DataCtor n)


-- | Get the type of a data constructor.
typeOfDataCtor :: DataDef n -> DataCtor n -> Type n
typeOfDataCtor def ctor
        = foldr TForall
                (foldr tFun (dataCtorResultType ctor)
                            (dataCtorFieldTypes ctor))
                (dataDefParams def)
