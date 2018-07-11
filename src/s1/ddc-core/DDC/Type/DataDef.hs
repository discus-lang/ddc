
-- | Algebraic data type definitions.
module DDC.Type.DataDef
        ( DataDef    (..)
        , mapTypeOfDataDef
        , kindOfDataDef
        , dataTypeOfDataDef
        , dataCtorNamesOfDataDef
        , makeDataDefAlg
        , makeDataDefAbs

        -- * Data type definition table
        , DataDefs   (..)
        , DataMode   (..)
        , dataDefOfDataType
        , emptyDataDefs
        , insertDataDef
        , unionDataDefs
        , fromListDataDefs

        , DataType   (..)
        , kindOfDataType
        , lookupModeOfDataType

        , DataCtor   (..)
        , mapTypeOfDataCtor
        , typeOfDataCtor)
where
import DDC.Core.Module.Name
import DDC.Type.Exp
import DDC.Type.Exp.Simple.Compounds
import Data.Map                         (Map)
import qualified Data.Map.Strict        as Map
import Data.Maybe
import Control.Monad
import Control.DeepSeq


---------------------------------------------------------------------------------------------------
-- | The definition of a single data type.
data DataDef n
        = DataDef
        { -- | Module the type is defined in.
          dataDefModuleName     :: !ModuleName

          -- | Name of the data type.
        , dataDefTypeName       :: !n

          -- | Binders for type parameters.
        , dataDefParams         :: ![Bind n]

          -- | Constructors of the data type,
          --   or Nothing if the data type is algebraic but there are too many
          --      constructors to list (like with `Int`).
        , dataDefCtors          :: !(Maybe [DataCtor n])

          -- | Whether the data type is algebraic.
          --   These can be deconstructed with 'case' expressions.
        , dataDefIsAlgebraic    :: Bool }
        deriving Show


instance NFData n => NFData (DataDef n) where
 rnf !def
        =       rnf (dataDefTypeName    def)
        `seq`   rnf (dataDefParams      def)
        `seq`   rnf (dataDefCtors       def)
        `seq`   rnf (dataDefIsAlgebraic def)


-- | Apply a function to all types in a data def.
mapTypeOfDataDef :: (Type n -> Type n) -> DataDef n -> DataDef n
mapTypeOfDataDef f def
 = def { dataDefCtors   = fmap (map (mapTypeOfDataCtor f)) (dataDefCtors def) }


-- | Get the kind of the type constructor defined by a `DataDef`.
kindOfDataDef :: DataDef n -> Kind n
kindOfDataDef def
 = let  ksParam = map typeOfBind $ dataDefParams def
   in   kFuns ksParam kData


-- | Get the type associated with a data definition,
--   that is, the type produced by the constructors.
dataTypeOfDataDef :: DataDef n -> Type n
dataTypeOfDataDef def
 = let  usParam = takeSubstBoundsOfBinds $ dataDefParams def
        tc      = TyConBound (dataDefTypeName def)
   in   tApps (TCon tc) (map TVar usParam)


-- | Get the list of data constructor names that this type defines,
--   or Nothing if there are too many to list.
dataCtorNamesOfDataDef :: DataDef n -> Maybe [n]
dataCtorNamesOfDataDef def
 = case dataDefCtors def of
        Nothing         -> Nothing
        Just ctors      -> Just $ map dataCtorName ctors


-- | Shortcut for constructing a `DataDef` for an algebraic type.
--
--   Values of algebraic type can be deconstructed with case-expressions.
makeDataDefAlg
        :: ModuleName   -- ^ Module the data type is defined in.
        -> n            -- ^ Name of data type.
        -> [Bind n]     -- ^ Type parameters.
        -> Maybe [(n, [Type n])]
                        -- ^ Constructor names and field types,
                        --      or `Nothing` if there are too many to list.
        -> DataDef n

makeDataDefAlg modName nData bsParam Nothing
        = DataDef
        { dataDefModuleName     = modName
        , dataDefTypeName       = nData
        , dataDefParams         = bsParam
        , dataDefCtors          = Nothing
        , dataDefIsAlgebraic    = True }

makeDataDefAlg modName nData bsParam (Just ntsField)
 = let  usParam = takeSubstBoundsOfBinds bsParam
        tc      = TyConBound nData
        tResult = tApps (TCon tc) (map TVar usParam)

        ctors   = [ DataCtor modName n tag tsField tResult nData bsParam
                            | tag          <- [0..]
                            | (n, tsField) <- ntsField]
   in   DataDef
        { dataDefModuleName     = modName
        , dataDefTypeName       = nData
        , dataDefParams         = bsParam
        , dataDefCtors          = Just ctors
        , dataDefIsAlgebraic    = True }


-- | Shortcut for constructing a `DataDef` for an abstract type.
--
--   Values of abstract type cannot be deconstructed with case-expressions.
makeDataDefAbs :: ModuleName -> n -> [Bind n] -> DataDef n
makeDataDefAbs modName nData bsParam
 = DataDef
        { dataDefModuleName     = modName
        , dataDefTypeName       = nData
        , dataDefParams         = bsParam
        , dataDefCtors          = Just []
        , dataDefIsAlgebraic    = False }


-- DataDefs ---------------------------------------------------------------------------------------
-- | A table of data type definitions,
--   unpacked into type and data constructors so we can find them easily.
data DataDefs n
        = DataDefs
        { dataDefsTypes :: !(Map n (DataType n))
        , dataDefsCtors :: !(Map n (DataCtor n)) }
        deriving Show


-- | The mode of a data type records how many data constructors there are.
--   This can be set to 'Large' for large primitive types like Int and Float.
--   In this case we don't ever expect them all to be enumerated
--   as case alternatives.
data DataMode n
        = DataModeSingle
        | DataModeSmall ![n]
        | DataModeLarge
        deriving Show


-- | Describes a data type constructor, used in the `DataDefs` table.
data DataType n
        = DataType
        { -- | Module the data type is defined in.
          dataTypeModuleName    :: !ModuleName

          -- | Name of data type constructor.
        , dataTypeName          :: !n

          -- | Kinds of type parameters to constructor.
        , dataTypeParams        :: ![Bind n]

          -- | Data constructors of this data type.
        , dataTypeCtors         :: !(Maybe [DataCtor n])

          -- | Names of data constructors of this data type,
          --   or `Nothing` if it has infinitely many constructors.
        , dataTypeMode          :: !(DataMode n)

          -- | Whether the data type is algebraic.
        , dataTypeIsAlgebraic   :: Bool }
        deriving Show



-- | Describes a data constructor, used in the `DataDefs` table.
data DataCtor n
        = DataCtor
        { -- | Module the data constructor is defined in.
          dataCtorModuleName    :: !ModuleName

          -- | Constructor name.
        , dataCtorName          :: !n

          -- | Tag of constructor (order in data type declaration)
        , dataCtorTag           :: !Integer

          -- | Field types of constructor.
        , dataCtorFieldTypes    :: ![Type n]

          -- | Result type of constructor.
        , dataCtorResultType    :: !(Type n)

          -- | Name of result type of constructor.
        , dataCtorTypeName      :: !n

          -- | Parameters of data type
        , dataCtorTypeParams    :: ![Bind n] }
        deriving Show


-- TODO: The 'DataType' and 'DataDef' types have become almost identical
-- during refactoring. Eliminate this redundancy.
dataDefOfDataType :: DataType n -> DataDef n
dataDefOfDataType dt
        = DataDef
        { dataDefModuleName     = dataTypeModuleName dt
        , dataDefTypeName       = dataTypeName dt
        , dataDefParams         = dataTypeParams dt
        , dataDefCtors          = dataTypeCtors dt
        , dataDefIsAlgebraic    = dataTypeIsAlgebraic dt }


-- | Apply a function to all types in a data ctor.
mapTypeOfDataCtor :: (Type n -> Type n) -> DataCtor n -> DataCtor n
mapTypeOfDataCtor f ctor
 = ctor { dataCtorFieldTypes    = map f (dataCtorFieldTypes ctor)
        , dataCtorResultType    = f (dataCtorResultType ctor) }


-- | Get the type of `DataCtor`
typeOfDataCtor :: DataCtor n -> Type n
typeOfDataCtor ctor
 = let  Just t   = tFunOfList (  dataCtorFieldTypes ctor
                              ++ [dataCtorResultType ctor] )
   in   foldr TForall t (dataCtorTypeParams ctor)


instance NFData n => NFData (DataCtor n) where
 rnf (DataCtor mn n t fs tR nT bsParam)
  =     rnf mn `seq` rnf n `seq` rnf t
  `seq` rnf fs `seq` rnf tR `seq` rnf nT `seq` rnf bsParam


-- | An empty table of data type definitions.
emptyDataDefs :: DataDefs n
emptyDataDefs
        = DataDefs
        { dataDefsTypes = Map.empty
        , dataDefsCtors = Map.empty }


-- | Union two `DataDef` tables.
unionDataDefs :: Ord n => DataDefs n -> DataDefs n -> DataDefs n
unionDataDefs defs1 defs2
 = DataDefs
        { dataDefsTypes = Map.union (dataDefsTypes defs1) (dataDefsTypes defs2)
        , dataDefsCtors = Map.union (dataDefsCtors defs1) (dataDefsCtors defs2) }


-- | Insert a data type definition into some DataDefs.
insertDataDef  :: Ord n => DataDef  n -> DataDefs n -> DataDefs n
insertDataDef (DataDef modName nType bsParam mCtors isAlg) dataDefs
 = let  defType = DataType
                { dataTypeModuleName    = modName
                , dataTypeName          = nType
                , dataTypeParams        = bsParam
                , dataTypeCtors         = mCtors
                , dataTypeMode          = defMode
                , dataTypeIsAlgebraic   = isAlg }

        defMode = case mCtors of
                   Nothing    -> DataModeLarge
                   Just ctors -> DataModeSmall (map dataCtorName ctors)

   in   dataDefs
         { dataDefsTypes = Map.insert nType defType (dataDefsTypes dataDefs)
         , dataDefsCtors = Map.union (dataDefsCtors dataDefs)
                         $ Map.fromList [(n, def)
                                | def@(DataCtor _ n _ _ _ _ _) <- concat $ maybeToList mCtors ]}


-- | Build a `DataDefs` table from a list of `DataDef`
fromListDataDefs :: Ord n => [DataDef n] -> DataDefs n
fromListDataDefs defs
        = foldr insertDataDef emptyDataDefs defs


-- | Yield the list of data constructor names for some data type,
--   or `Nothing` for large types with too many constructors to list.
lookupModeOfDataType :: Ord n => n -> DataDefs n -> Maybe (DataMode n)
lookupModeOfDataType n defs
        = liftM dataTypeMode $ Map.lookup n (dataDefsTypes defs)


-- | Get the kind of the type constructor defined by a `DataDef`.
kindOfDataType :: DataType n -> Kind n
kindOfDataType def
 = let  ksParam = map typeOfBind $ dataTypeParams def
   in   kFuns ksParam kData

