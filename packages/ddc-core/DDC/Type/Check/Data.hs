
module DDC.Type.Check.Data
        (checkDataDefs)
where
import DDC.Type.Check.Error
import DDC.Type.Check.Config
import DDC.Type.DataDef
import DDC.Base.Pretty
import Data.Maybe
import Data.Set                 (Set)
import qualified Data.Set       as Set


-- | Check some data type definitions.
checkDataDefs 
        :: (Ord n, Show n, Pretty n)
        => Config n
        -> [DataDef n]
        -> ([ErrorData n], [DataDef n])

checkDataDefs _config defs 
 = checkDataDefs' 
        Set.empty Set.empty [] []
        defs


checkDataDefs'
        :: (Ord n, Show n, Pretty n)
        => Set n                -- ^ Names of existing data types.
        -> Set n                -- ^ Names of existing data constructor.
        -> [ErrorData n]        -- ^ Errors found so far.
        -> [DataDef n]          -- ^ Checked data defs.
        -> [DataDef n]          -- ^ Data defs still to check.
        -> ([ErrorData n], [DataDef n])

checkDataDefs' nsTypes nsCtors errs dsChecked ds
 -- We've checked all the defs.
 | []   <- ds
 = (reverse errs, reverse dsChecked)
 
 -- Keep checking defs.
 | d : ds' <- ds
 = case checkDataDef nsTypes nsCtors d of

    -- There are errors in this def.
    Left errs' 
     -> checkDataDefs'
                (Set.insert (dataDefTypeName d) nsTypes)
                (Set.fromList $ fromMaybe [] $ dataCtorNamesOfDataDef d)
                (errs ++ errs') dsChecked ds'

    -- This def is ok.
    Right d'   
     -> checkDataDefs'
                (Set.insert (dataDefTypeName d') nsTypes)
                (Set.fromList $ fromMaybe [] $ dataCtorNamesOfDataDef d)
                errs (d' : dsChecked) ds'

 | otherwise
 = error "ddc-core.checkDataDefs: bogus warning suppression"


-- DataDef --------------------------------------------------------------------
-- | Check a data type definition.
checkDataDef 
        :: (Ord n, Show n, Pretty n)
        => Set n                -- ^ Names of existing data types.
        -> Set n                -- ^ Names of existing data constructors.
        -> DataDef n            -- ^ Data type definition to check.
        -> Either [ErrorData n] (DataDef n)

checkDataDef nsTypes nsCtors def
        
 -- Check the data type name is not already defined.
 | Set.member (dataDefTypeName def) nsTypes
 = error "checkDef: dup type name"

 -- No data constructors to check.
 | Nothing      <- dataDefCtors def
 = Right def

 -- Check the data constructors.
 | Just ctors   <- dataDefCtors def
 = case checkDataCtors nsCtors [] [] ctors of
        Left errs       -> Left errs
        Right ctors'    -> Right $ def { dataDefCtors = Just ctors' }

 | otherwise
 = error "ddc-core.checkDataDef: bogus warning suppression"

 
-- Ctors ----------------------------------------------------------------------
-- | Check the data constructor definitions from a single data type.
checkDataCtors
        :: (Ord n, Show n, Pretty n)
        => Set n                -- ^ Names of existing data constructors.
        -> [ErrorData n]        -- ^ Errors found so far.
        -> [DataCtor  n]        -- ^ Checked constructor defs.
        -> [DataCtor  n]        -- ^ Constructor defs still to check.
        -> Either [ErrorData n] [DataCtor n]

checkDataCtors nsCtors errs csChecked cs
 -- We've checked all the constructors and there were no errors.
 | []   <- cs, []   <- errs
 = Right (reverse csChecked)

 -- We've checked all the constructors and there were errors with some of them.
 | []   <- cs
 = Left  (reverse errs)

 -- Keep checking constructors.
 | c : cs' <- cs
 = case checkDataCtor nsCtors c of
        Left  err -> checkDataCtors 
                        (Set.insert (dataCtorName c) nsCtors)
                        (err : errs) csChecked cs'
        
        Right c'  -> checkDataCtors 
                        (Set.insert (dataCtorName c') nsCtors)
                        errs (c' : csChecked) cs'

 | otherwise
 = error "ddc-core.checkDataCtors: bogus warning suppression"


-- Ctor -----------------------------------------------------------------------
-- | Check a single data constructor definition.
checkDataCtor 
        :: (Ord n, Show n, Pretty n)
        => Set n                -- ^ Names of existing data constructors.
        -> DataCtor n           -- ^ Data constructor to check.
        -> Either (ErrorData n) (DataCtor n)

checkDataCtor nsCtors ctor
        
        -- Check the constructor name is not already defined.
        | Set.member (dataCtorName ctor) nsCtors 
        = Left $ ErrorDataDupCtorName (dataCtorName ctor)

        -- This constructor looks ok.
        | otherwise
        = Right ctor

