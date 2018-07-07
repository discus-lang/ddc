
module DDC.Core.Check.Close
        (closeModuleWithOracle)
where
import DDC.Core.Module
import DDC.Core.Check.Base
import DDC.Core.Check.Exp
-- import DDC.Core.Check.Context
import DDC.Core.Check.Context.Oracle
-- import Control.Monad.IO.Class
-- import Data.Map                         (Map)
import Data.IORef
import qualified Data.Map.Strict        as Map


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
