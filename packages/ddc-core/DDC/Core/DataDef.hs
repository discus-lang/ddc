
module DDC.Core.DataDef
        ( DataDef    (..)
        , DataType   (..)
        , DataCtor   (..)

        , DataDefs
        , emptyDataDefs
        , insertDataDef
        , fromListDataDefs)
where
import DDC.Type.Exp
import Data.Map                 (Map)
import qualified Data.Map       as Map
import Control.Monad
import Data.Maybe


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
        , dataTypeCtorNames  :: Maybe [n] }


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
                , dataTypeCtorNames  = liftM (map fst) mCtors }

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



