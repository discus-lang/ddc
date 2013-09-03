
-- | Algebraic data type definitions.
module DDC.Type.DataDef
        ( DataDef    (..)
        , kindOfDataDef
        , dataTypeOfDataDef
        , makeDataDefAlg
        , makeDataDefAbs

        -- * Data type definition table
        , DataDefs   (..)
        

        , DataMode   (..)
        , emptyDataDefs
        , insertDataDef
        , unionDataDefs
        , fromListDataDefs
        
        , DataType   (..)
        , kindOfDataType
        , lookupModeOfDataType

        , DataCtor   (..)
        , typeOfDataCtor)
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

          -- | Constructors of the data type, 
          --   or Nothing if the data type is algbraic but there are too many 
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


-- | Get the kind of the type constructor defined by a `DataDef`.
kindOfDataDef :: DataDef n -> Kind n
kindOfDataDef def
 = let  ksParam = map typeOfBind $ dataDefParams def
   in   kFuns ksParam kData
   

-- | Get the the type associated with a data definition, 
--   that is, the type produced by the constructors.
dataTypeOfDataDef :: DataDef n -> Type n
dataTypeOfDataDef def
 = let  usParam = takeSubstBoundsOfBinds $ dataDefParams def
        ksParam = map typeOfBind $ dataDefParams def
        tc      = TyConBound (UName (dataDefTypeName def))
                             (kFuns ksParam kData)
   in   tApps (TCon tc) (map TVar usParam)
                                        

-- | Shortcut for constructing a `DataDef` for an algebraic type.
--
--   Values of algebraic type can be deconstructed with case-expressions.
makeDataDefAlg 
        :: n            -- ^ Name of data type.
        -> [Bind n]     -- ^ Type parameters.
        -> Maybe [(n, [Type n])]        
                        -- ^ Constructor names and field types,
                        --      or `Nothing` if there are too many to list.
        -> DataDef n

makeDataDefAlg nData bsParam Nothing 
        = DataDef
        { dataDefTypeName       = nData
        , dataDefParams         = bsParam
        , dataDefCtors          = Nothing 
        , dataDefIsAlgebraic    = True }

makeDataDefAlg nData bsParam (Just ntsField)
 = let  usParam = takeSubstBoundsOfBinds bsParam
        ksParam = map typeOfBind bsParam
        tc      = TyConBound (UName nData) 
                             (kFuns ksParam kData)

        tResult = tApps (TCon tc) (map TVar usParam)
   
        ctors   = [ DataCtor n tag tsField tResult nData bsParam
                            | tag          <- [0..]
                            | (n, tsField) <- ntsField] 
   in   DataDef
        { dataDefTypeName       = nData
        , dataDefParams         = bsParam
        , dataDefCtors          = Just ctors 
        , dataDefIsAlgebraic    = True }
  

-- | Shortcut for constructing a `DataDef` for an abstract type.
--
--   Values of abstract type cannot be deconstructed with case-expressions.
makeDataDefAbs :: n -> [Bind n] -> DataDef n
makeDataDefAbs nData bsParam
 = DataDef
        { dataDefTypeName       = nData
        , dataDefParams         = bsParam
        , dataDefCtors          = Just []
        , dataDefIsAlgebraic    = False }


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
        , dataTypeMode       :: !(DataMode n) 

          -- | Whether the data type is algebraic.
        , dataTypeIsAlgebraic :: Bool }
        deriving Show


-- | Describes a data constructor, used in the `DataDefs` table.
data DataCtor n
        = DataCtor
        { -- | Name of data constructor.
          dataCtorName        :: !n

          -- | Tag of constructor (order in data type declaration)
        , dataCtorTag         :: !Integer

          -- | Field types of constructor.
        , dataCtorFieldTypes  :: ![Type n]

          -- | Result type of constructor.
        , dataCtorResultType  :: !(Type n)

          -- | Name of result type of constructor.
        , dataCtorTypeName    :: !n 

          -- | Parameters of data type 
        , dataCtorTypeParams :: ![Bind n] }
        deriving Show


-- | Get the type of `DataCtor`
typeOfDataCtor :: DataCtor n -> Type n
typeOfDataCtor ctor
 = let  Just t   = tFunOfList (  dataCtorFieldTypes ctor 
                              ++ [dataCtorResultType ctor] )
   in   foldr TForall t (dataCtorTypeParams ctor)


instance NFData n => NFData (DataCtor n) where
 rnf (DataCtor n t fs tR nT bsParam)
  = rnf n `seq` rnf t `seq` rnf fs `seq` rnf tR `seq` rnf nT `seq` rnf bsParam


-- | An empty table of data type definitions.
emptyDataDefs :: DataDefs n
emptyDataDefs
        = DataDefs
        { dataDefsTypes = Map.empty
        , dataDefsCtors = Map.empty }


-- | Insert a data type definition into some DataDefs.
insertDataDef  :: Ord n => DataDef  n -> DataDefs n -> DataDefs n
insertDataDef (DataDef nType bsParam mCtors isAlg) dataDefs
 = let  defType = DataType
                { dataTypeName        = nType
                , dataTypeParams      = bsParam
                , dataTypeMode        = defMode 
                , dataTypeIsAlgebraic = isAlg }

        defMode = case mCtors of
                   Nothing    -> DataModeLarge
                   Just ctors -> DataModeSmall (map dataCtorName ctors)

   in   dataDefs
         { dataDefsTypes = Map.insert nType defType (dataDefsTypes dataDefs)
         , dataDefsCtors = Map.union (dataDefsCtors dataDefs)
                         $ Map.fromList [(n, def) 
                                | def@(DataCtor n _ _ _ _ _) <- concat $ maybeToList mCtors ]}


-- | Union two `DataDef` tables.
unionDataDefs :: Ord n => DataDefs n -> DataDefs n -> DataDefs n
unionDataDefs defs1 defs2
        = DataDefs
        { dataDefsTypes = Map.union (dataDefsTypes defs1) (dataDefsTypes defs2)
        , dataDefsCtors = Map.union (dataDefsCtors defs1) (dataDefsCtors defs2) }



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

