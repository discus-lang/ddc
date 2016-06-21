
module DDC.Type.Check.Data
        (checkDataDefs)
where
import DDC.Type.Check.Error
import DDC.Type.Check.Config
import DDC.Type.Exp.Simple
import DDC.Type.DataDef
import DDC.Base.Pretty
import Data.Maybe
import DDC.Core.Env.EnvT                (EnvT)
import Data.Set                         (Set)
import qualified DDC.Type.Env           as Env
import qualified Data.Set               as Set
import qualified Data.Map               as Map


-------------------------------------------------------------------------------
-- | Check some data type definitions.
checkDataDefs 
        :: (Ord n, Show n, Pretty n)
        => Config n
        -> EnvT   n
        -> [DataDef n]
        -> ([ErrorData n], [DataDef n])

checkDataDefs config env defs 
 = let
        -- Primitive type constructors.
        primTypeCtors
                = Set.fromList
                $ Map.keys $ Env.envMap $ configPrimKinds config

        -- Primitive data type constructors
        primDataTypeCtors  
                = Set.fromList 
                $ Map.keys $ dataDefsTypes $ configDataDefs config

        -- Primitive data constructors
        primDataCtors   
                = Set.fromList 
                $ Map.keys $ dataDefsCtors $ configDataDefs config

   in   checkDataDefs' env
                (Set.union primTypeCtors primDataTypeCtors)
                primDataCtors
                [] []
                defs


checkDataDefs'
        :: (Ord n, Show n, Pretty n)
        => EnvT n               -- ^ Type equations.
        -> Set n                -- ^ Names of existing data types.
        -> Set n                -- ^ Names of existing data constructor.
        -> [ErrorData n]        -- ^ Errors found so far.
        -> [DataDef n]          -- ^ Checked data defs.
        -> [DataDef n]          -- ^ Data defs still to check.
        -> ([ErrorData n], [DataDef n])

checkDataDefs' env nsTypes nsCtors errs dsChecked ds
 -- We've checked all the defs.
 | []   <- ds
 = (reverse errs, reverse dsChecked)
 
 -- Keep checking defs.
 | d : ds' <- ds
 = case checkDataDef env nsTypes nsCtors d of

    -- There are errors in this def.
    Left errs' 
     -> checkDataDefs' env
                (Set.insert (dataDefTypeName d) nsTypes)
                (Set.fromList $ fromMaybe [] $ dataCtorNamesOfDataDef d)
                (errs ++ errs') dsChecked ds'

    -- This def is ok.
    Right d'   
     -> checkDataDefs' env
                (Set.insert (dataDefTypeName d') nsTypes)
                (Set.fromList $ fromMaybe [] $ dataCtorNamesOfDataDef d)
                errs (d' : dsChecked) ds'

 | otherwise
 = error "ddc-core.checkDataDefs: bogus warning suppression"


-- DataDef --------------------------------------------------------------------
-- | Check a data type definition.
checkDataDef 
        :: (Ord n, Show n, Pretty n)
        => EnvT n               -- ^ Environment of types.
        -> Set n                -- ^ Names of existing data types.
        -> Set n                -- ^ Names of existing data constructors.
        -> DataDef n            -- ^ Data type definition to check.
        -> Either [ErrorData n] (DataDef n)

checkDataDef env nsTypes nsCtors def
        
 -- Check the data type name is not already defined.
 | Set.member (dataDefTypeName def) nsTypes
 = Left [ErrorDataDupTypeName (dataDefTypeName def)]

 -- No data constructors to check.
 | Nothing      <- dataDefCtors def
 = Right def

 -- Check the data constructors.
 | Just ctors   <- dataDefCtors def
 = case checkDataCtors env nsCtors [] def [] ctors of
        Left errs       -> Left errs
        Right ctors'    -> Right $ def { dataDefCtors = Just ctors' }

 | otherwise
 = error "ddc-core.checkDataDef: bogus warning suppression"

 
-- Ctors ----------------------------------------------------------------------
-- | Check the data constructor definitions from a single data type.
checkDataCtors
        :: (Ord n, Show n, Pretty n)
        => EnvT n               -- ^ Environment of types
        -> Set n                -- ^ Names of existing data constructors.
        -> [ErrorData n]        -- ^ Errors found so far.
        -> DataDef n            -- ^ The DataDef these constructors relate to.
        -> [DataCtor  n]        -- ^ Checked constructor defs.
        -> [DataCtor  n]        -- ^ Constructor defs still to check.
        -> Either [ErrorData n] [DataCtor n]

checkDataCtors env nsCtors errs def csChecked cs
 -- We've checked all the constructors and there were no errors.
 | []   <- cs, []   <- errs
 = Right (reverse csChecked)

 -- We've checked all the constructors and there were errors with some of them.
 | []   <- cs
 = Left  (reverse errs)

 -- Keep checking constructors.
 | c : cs' <- cs
 = case checkDataCtor env nsCtors def c of
        Left  err -> checkDataCtors env
                        (Set.insert (dataCtorName c) nsCtors)
                        (err : errs) def csChecked cs'
        
        Right c'  -> checkDataCtors env
                        (Set.insert (dataCtorName c') nsCtors)
                        errs         def (c' : csChecked) cs'

 | otherwise
 = error "ddc-core.checkDataCtors: bogus warning suppression"


-- Ctor -----------------------------------------------------------------------
-- | Check a single data constructor definition.
checkDataCtor 
        :: (Ord n, Show n, Pretty n)
        => EnvT n               -- ^ Environment of types.
        -> Set n                -- ^ Names of existing data constructors.
        -> DataDef  n           -- ^ Def of data type for this constructor.
        -> DataCtor n           -- ^ Data constructor to check.
        -> Either (ErrorData n) (DataCtor n)

checkDataCtor env nsCtors def ctor
        
 -- Check the constructor name is not already defined.
 | Set.member (dataCtorName ctor) nsCtors 
 = Left $ ErrorDataDupCtorName (dataCtorName ctor)

 -- Check that the constructor produces a value of the associated data type.
 | not $ equivT env (dataTypeOfDataDef def) (dataCtorResultType ctor)
 = Left $ ErrorDataWrongResult 
                (dataCtorName ctor)
                (dataCtorResultType ctor) (dataTypeOfDataDef def)

 -- This constructor looks ok.
 | otherwise
 = Right ctor



