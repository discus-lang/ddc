
-- | Algebraic data type definitions.
module DDC.Core.DataDef
        ( DataDef    (..)
        , DataType   (..)
        , DataMode   (..)
        , DataCtor   (..)

        , DataDefs
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

-- | Some data type declarations.
data DataDefs n
        = DataDefs
        { dataDefsTypes :: Map n (DataType n)
        , dataDefsCtors :: Map n (DataCtor n) }


-- | A data type declaration.
data DataDef n
        = DataDef
        { dataDefTypeName       :: n
        , dataDefParamKinds     :: [Kind n]
        , dataDefCtors          :: Maybe [(n, [Type n])] }


-- | Describes a data type.
data DataType n
        = DataType 
        { -- | Name of data type constructor.
          dataTypeName       :: n

          -- | Kinds of type parameters of constructor.
        , dataTypeParamKinds :: [Kind n]

          -- | Names of data constructors of this data type,
          --   or Nothing if it has infinitely many constructors.
        , dataTypeMode       :: DataMode n }


-- | The mode of a data type records how many data constructors there are.
--   This can be set to 'Large' for large primitive types like Int and Float.
--   In this case we don't ever expect them all to be enumerated
--   as case alternatives.
data DataMode n
        = DataModeSmall [n]
        | DataModeLarge


-- | Describes a data constructor.
data DataCtor n
        = DataCtor
        { -- | Name of data constructor.
          dataCtorName       :: n

          -- | Field types of constructor.
        , dataCtorFieldTypes :: [Type n]

          -- | Name of result type of constructor.
        , dataCtorTypeName   :: n }


-- | An empty set of data defs.
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


-- | Build a DataDefs from a list of DataDef
fromListDataDefs :: Ord n => [DataDef n] -> DataDefs n
fromListDataDefs defs
        = foldr insertDataDef emptyDataDefs defs



-- | Get the list of data constructor names for some data type, 
--   or `Nothing` if there are infinitely many constructors.
lookupModeOfDataType :: Ord n => n -> DataDefs n -> Maybe (DataMode n)
lookupModeOfDataType n defs
        = liftM dataTypeMode $ Map.lookup n (dataDefsTypes defs)


