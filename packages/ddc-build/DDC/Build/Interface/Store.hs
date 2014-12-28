
module DDC.Build.Interface.Store
        ( Store
        , new, wrap, load
        , Super (..), findSuper)
where
import DDC.Build.Interface.Base
import DDC.Build.Interface.Load
import DDC.Core.Module
import DDC.Type.Exp
import System.Directory
import Data.IORef
import Data.Maybe
import Data.Map                         (Map)
import qualified DDC.Core.Tetra         as E
import qualified DDC.Core.Salt          as A
import qualified Data.Map               as Map


-- | Abstract API to a collection of module interfaces.
--
--   This lives in IO land because in future we want to demand-load the
--   inferface files as needed, rather than loading the full dependency
--   tree. Keeping it in IO means that callers must also be in IO.
data Store
        = Store
        { -- | Lookup the definition of the given top-level super, 
          --   from one or more of the provided modules.
          storeSupers   :: IORef (Map ModuleName (Map E.Name Super)) }


---------------------------------------------------------------------------------------------------
-- | Interface for some top-level super.
data Super
        = Super
        { -- | Name of the super.
          superName             :: E.Name

          -- | Where this super is defined.
        , superModuleName       :: ModuleName

          -- | Tetra type for the super.
        , superTetraType        :: Type E.Name

          -- | Salt type for the super.
        , superSaltType         :: Type A.Name }


---------------------------------------------------------------------------------------------------
-- | An empty interface store.
new :: IO Store
new
 = do   refSupers       <- newIORef Map.empty
        return  $ Store { storeSupers = refSupers }


-- | Add a pre-loaded interface file to the store.
wrap    :: Store -> InterfaceAA -> IO ()
wrap store int
 = do   supers  <- readIORef (storeSupers store)
        writeIORef (storeSupers store) 
                $ Map.insert (interfaceModuleName int)
                             (supersOfInterface   int)
                             supers


-- | Load a new interface into the store.
load    :: Store -> FilePath -> IO (Maybe Error)
load store filePath
 = do   timeStamp  <- getModificationTime filePath
        str        <- readFile filePath
        case loadInterface filePath timeStamp str of
         Left err  
          ->    return $ Just err

         Right int 
          -> do wrap store int
                return Nothing


-- | Extract a map of super interfaces from the given module interface.
supersOfInterface :: InterfaceAA -> Map E.Name Super
supersOfInterface int
 | Just mmTetra <- interfaceTetraModule int
 , Just mmSalt  <- interfaceSaltModule  int
 = let  
        -- Names and Tetra types of all supers exported by the module.
        ntsTetra    
         = [ (n, t)     | (n, esrc)     <- moduleExportValues mmTetra
                        , let Just t    =  takeTypeOfExportSource esrc ]

        -- Names and Salt  types of all supers exported by the module.
        ntsSalt 
         = Map.fromList
           [ (n, t)     | (n, esrc)     <- moduleExportValues mmSalt
                        , let Just t    =  takeTypeOfExportSource esrc ]

        makeSuper n tTetra
         = case n of
            E.NameVar s 
                -> Just $ Super
                { superName       = n
                , superModuleName = moduleName mmTetra
                , superTetraType  = tTetra
                , superSaltType   = let Just t = Map.lookup (A.NameVar s) ntsSalt  in t }
            _   -> Nothing

   in   Map.fromList   
          [ (n, super)  | (n, tTetra)    <- ntsTetra 
                        , let Just super = makeSuper n tTetra ]

 | otherwise
 = Map.empty


---------------------------------------------------------------------------------------------------
-- | See if a super is defined in any of the given modules, and if so
--   return the module name and super type.
--
--   NOTE: This function returns an IO [Super] in preparation for the case
--   where we load data from interface files on demand. We want to ensure
--   that the caller is also in IO, to make the refactoring easier later.
--
findSuper
        :: Store
        -> E.Name               -- ^ Name of desired super.
        -> [ModuleName]         -- ^ Names of modules to search.
        -> IO [Super]

findSuper store n modNames 
 = do   supers  <- readIORef (storeSupers store)
        return $ mapMaybe
                (\modName -> do
                        nSupers <- Map.lookup modName supers
                        Map.lookup n nSupers)
                modNames

