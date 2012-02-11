
-- | Algebraic data type definitions.
module DDC.Core.DataDef
        ( DataDef    (..)

        -- * Data type definition table
        , DataDefs   (..)
        , DataMode   (..)
        , DataType   (..)
        , DataCtor   (..)

        , emptyDataDefs
        , insertDataDef
        , fromListDataDefs
        , lookupModeOfDataType)
where
import DDC.Type.Exp
import Data.Map                 (Map)
import qualified Data.Map       as Map
import Data.Maybe
import Control.Monad


-- | The definition of a single data type.
data DataDef n
        = DataDef
        { -- | Name of the data type.
          dataDefTypeName       :: n

          -- | Kinds of type parameters.
        , dataDefParamKinds     :: [Kind n]

          -- | Constructors of is data type, or Nothing if there are
          --   too many to list (like with `Int`).
        , dataDefCtors          :: Maybe [(n, [Type n])] }



-- DataDefs -------------------------------------------------------------------
-- | A table of data type definitions,
--   unpacked into type and data constructors so we can find them easily.
data DataDefs n
        = DataDefs
        { dataDefsTypes :: Map n (DataType n)
        , dataDefsCtors :: Map n (DataCtor n) }


-- | The mode of a data type records how many data constructors there are.
--   This can be set to 'Large' for large primitive types like Int and Float.
--   In this case we don't ever expect them all to be enumerated
--   as case alternatives.
data DataMode n
        = DataModeSmall [n]
        | DataModeLarge


-- | Describes a data type constructor, used in the `DataDefs` table.
data DataType n
        = DataType 
        { -- | Name of data type constructor.
          dataTypeName       :: n

          -- | Kinds of type parameters to constructor.
        , dataTypeParamKinds :: [Kind n]

          -- | Names of data constructors of this data type,
          --   or `Nothing` if it has infinitely many constructors.
        , dataTypeMode       :: DataMode n }


-- | Describes a data constructor, used in the `DataDefs` table.
data DataCtor n
        = DataCtor
        { -- | Name of data constructor.
          dataCtorName       :: n

          -- | Field types of constructor.
        , dataCtorFieldTypes :: [Type n]

          -- | Name of result type of constructor.
        , dataCtorTypeName   :: n }



-- | An empty table of data type definitions.
emptyDataDefs :: DataDefs n
emptyDataDefs
        = DataDefs
        { dataDefsTypes = Map.empty
        , dataDefsCtors = Map.empty }


-- | Insert a data type definition into some DataDefs.
insertDataDef  :: Ord n => DataDef  n -> DataDefs n -> DataDefs n
insertDataDef (DataDef nType ks mCtors) dataDefs
 = let  defType = DataType
                { dataTypeName       = nType
                , dataTypeParamKinds = ks
                , dataTypeMode       = defMode }

        defMode = case mCtors of
                   Nothing    -> DataModeLarge
                   Just ctors -> DataModeSmall (map fst ctors)

        makeDefCtor (nCtor, tsFields)
                = DataCtor
                { dataCtorName       = nCtor
                , dataCtorFieldTypes = tsFields
                , dataCtorTypeName   = nType }

        defCtors = case mCtors of
                    Nothing  -> Nothing
                    Just cs  -> Just $ map makeDefCtor cs

   in   dataDefs
         { dataDefsTypes = Map.insert nType defType (dataDefsTypes dataDefs)
         , dataDefsCtors = Map.union (dataDefsCtors dataDefs)
                         $ Map.fromList [(n, def) | def@(DataCtor n _ _) 
                                          <- concat $ maybeToList defCtors] }


-- | Build a `DataDefs` table from a list of `DataDef`
fromListDataDefs :: Ord n => [DataDef n] -> DataDefs n
fromListDataDefs defs
        = foldr insertDataDef emptyDataDefs defs



-- | Yield the list of data constructor names for some data type, 
--   or `Nothing` for large types with too many constructors to list.
lookupModeOfDataType :: Ord n => n -> DataDefs n -> Maybe (DataMode n)
lookupModeOfDataType n defs
        = liftM dataTypeMode $ Map.lookup n (dataDefsTypes defs)


