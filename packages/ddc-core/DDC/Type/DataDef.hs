
-- | Algebraic data type definitions.
module DDC.Type.DataDef
        ( DataDef    (..)
        , dataTypeOfDataDef

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
import DDC.Type.Compounds
import Data.Map                         (Map)
import qualified Data.Map.Strict        as Map
import Data.Maybe
import Control.Monad
import Control.DeepSeq


-- | The definition of a single data type.
data DataDef n
        = DataDef
        { -- | Name of the data type.
          dataDefTypeName       :: !n

          -- | Binders for type parameters.
        , dataDefParams         :: ![Bind n]

          -- | Constructors of the data type, or Nothing if there are
          --   too many to list (like with `Int`).
        , dataDefCtors          :: !(Maybe [(n, [Type n])]) }
        deriving Show


instance NFData n => NFData (DataDef n) where
 rnf !def
        =       rnf (dataDefTypeName def)
        `seq`   rnf (dataDefParams   def)
        `seq`   rnf (dataDefCtors    def)


-- | Get the the type associated with a data definition, 
--   that is, the type produced by the constructors.
dataTypeOfDataDef :: DataDef n -> Maybe (Type n)
dataTypeOfDataDef def
 | Just usParam <- sequence 
                $  map takeSubstBoundOfBind 
                $  dataDefParams def
 = let  ksParam = map typeOfBind $ dataDefParams def
        tc      = TyConBound (UName (dataDefTypeName def))
                             (kFuns ksParam kData)
   in   Just $ tApps (TCon tc) (map TVar usParam)
                                        
 | otherwise
 = Nothing                      


-- DataDefs -------------------------------------------------------------------
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
        = DataModeSmall ![n]
        | DataModeLarge
        deriving Show


-- | Describes a data type constructor, used in the `DataDefs` table.
data DataType n
        = DataType 
        { -- | Name of data type constructor.
          dataTypeName       :: !n

          -- | Kinds of type parameters to constructor.
        , dataTypeParams     :: ![Bind n]

          -- | Names of data constructors of this data type,
          --   or `Nothing` if it has infinitely many constructors.
        , dataTypeMode       :: !(DataMode n) }
        deriving Show


-- | Describes a data constructor, used in the `DataDefs` table.
data DataCtor n
        = DataCtor
        { -- | Name of data constructor.
          dataCtorName       :: !n

          -- | Tag of constructor (order in data type declaration)
        , dataCtorTag        :: !Integer

          -- | Field types of constructor.
        , dataCtorFieldTypes :: ![Type n]

          -- | Name of result type of constructor.
        , dataCtorTypeName   :: !n }
        deriving Show


-- | An empty table of data type definitions.
emptyDataDefs :: DataDefs n
emptyDataDefs
        = DataDefs
        { dataDefsTypes = Map.empty
        , dataDefsCtors = Map.empty }


-- | Insert a data type definition into some DataDefs.
insertDataDef  :: Ord n => DataDef  n -> DataDefs n -> DataDefs n
insertDataDef (DataDef nType bsParam mCtors) dataDefs
 = let  defType = DataType
                { dataTypeName       = nType
                , dataTypeParams     = bsParam
                , dataTypeMode       = defMode }

        defMode = case mCtors of
                   Nothing    -> DataModeLarge
                   Just ctors -> DataModeSmall (map fst ctors)

        makeDefCtor tag (nCtor, tsFields)
                = DataCtor
                { dataCtorName       = nCtor
                , dataCtorTag        = tag
                , dataCtorFieldTypes = tsFields
                , dataCtorTypeName   = nType }

        defCtors = case mCtors of
                    Nothing  -> Nothing
                    Just cs  -> Just $ zipWith makeDefCtor [0..] cs

   in   dataDefs
         { dataDefsTypes = Map.insert nType defType (dataDefsTypes dataDefs)
         , dataDefsCtors = Map.union (dataDefsCtors dataDefs)
                         $ Map.fromList [(n, def) 
                                | def@(DataCtor n _ _ _) <- concat $ maybeToList defCtors ]}


-- | Build a `DataDefs` table from a list of `DataDef`
fromListDataDefs :: Ord n => [DataDef n] -> DataDefs n
fromListDataDefs defs
        = foldr insertDataDef emptyDataDefs defs



-- | Yield the list of data constructor names for some data type, 
--   or `Nothing` for large types with too many constructors to list.
lookupModeOfDataType :: Ord n => n -> DataDefs n -> Maybe (DataMode n)
lookupModeOfDataType n defs
        = liftM dataTypeMode $ Map.lookup n (dataDefsTypes defs)


