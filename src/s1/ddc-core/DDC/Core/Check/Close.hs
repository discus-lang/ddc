
module DDC.Core.Check.Close
        ( closeModuleWithOracle
        , closeImportThings)
where
import DDC.Core.Module
import DDC.Core.Check.Base
import DDC.Core.Check.Exp
import DDC.Core.Interface.Store
-- import DDC.Core.Check.Context
import DDC.Core.Check.Context.Oracle
-- import Control.Monad.IO.Class
import Data.Map                         (Map)
import qualified DDC.Core.Collect.FreeT as FreeT
import Data.IORef
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set

---------------------------------------------------------------------------------------------------
-- | Use the cache in the given interface oracle to add import declarations
--   to the module that cover all the free variable. The resulting module
--   will have no 'import module' specifications, and no need for them.
closeModuleWithOracle
        :: (Ord n, Show n, Show a)
        => Oracle n
        -> Module (AnTEC a n) n
        -> CheckM a n (Module (AnTEC a n) n)

closeModuleWithOracle oracle mm
 = do
        dataTypesByTyCon <- liftIO $ readIORef (oracleCacheDataTypesByTyCon oracle)
        typeSynsByTyCon  <- liftIO $ readIORef (oracleCacheTypeSynsByTyCon oracle)
        _dataCtorsByDaCon <- liftIO $ readIORef (oracleCacheDataCtorsByDaCon oracle)
        valuesByName     <- liftIO $ readIORef (oracleCacheValuesByName oracle)

        return $ mm
         { moduleImportModules
                = []

         , moduleImportDataDefs
                =  moduleImportDataDefs mm
                ++ (Map.toList $ Map.map dataDefOfDataType dataTypesByTyCon)

         , moduleImportTypeDefs
                =  moduleImportTypeDefs mm ++ Map.toList typeSynsByTyCon

         , moduleImportValues
                =  moduleImportValues   mm ++ Map.toList valuesByName
         }


---------------------------------------------------------------------------------------------------
-- | Things that can be imported into a module.
data ImportThing n
        = ImportThingDataType   n (DataType n)
        | ImportThingDataCtor   n (DataCtor n)
        | ImportThingForeign    n (ImportType n  (Kind n))
        | ImportThingSyn        n (Kind n) (Type n)
        | ImportThingValue      n (ImportValue n (Type n))
        deriving Show

closeImportThings
        :: Ord n
        => KindEnv n
        -> Store n
        -> Map n (ImportThing n)
        -> IO (Map n (ImportThing n))

closeImportThings kenv _store mpThings
 = go   (Set.fromList $ Map.keys mpThings)
        Map.empty
        mpThings
 where
        go nsHave mpDone mpToCheck
         = do   -- Names of types needed by the to-check things.
                let nsNeeded    = Set.unions
                                $ map (supportOfImportThing kenv)
                                $ Map.elems mpToCheck

                -- Names of types we need to import that we don't already have.
                let nsImport    = Set.difference nsNeeded nsHave

                if Set.null nsImport
                 -- We've already got a closed set of things.
                 then return (Map.union mpDone mpToCheck)

                 -- We need to chase down more imports.
                 else do
                        mpImport <- fmap Map.unions $ mapM chase $ Set.toList nsImport
                        go (Set.union nsHave nsImport)
                           (Map.union mpDone mpToCheck)
                           mpImport

        -- FIXME: finish this.
        chase _n
         = return Map.empty

---------------------------------------------------------------------------------------------------
-- | Compute the support set of an import thing.
supportOfImportThing :: Ord n => KindEnv n -> ImportThing n -> Set n
supportOfImportThing kenv it
 = case it of
        ImportThingDataType{}   -> Set.empty
        ImportThingDataCtor{}   -> Set.empty
        ImportThingForeign{}    -> Set.empty
        ImportThingSyn _ _ t    -> supportOfType kenv t
        ImportThingValue{}      -> Set.empty


-- | Names of type constructors used in the given type.
supportOfType :: Ord n => KindEnv n -> Type n -> Set n
supportOfType kenv tt
 = let  (_, bs) = FreeT.freeVarConT kenv tt
        ns      = [ n | UName n <- Set.toList bs ]
   in   Set.fromList ns


