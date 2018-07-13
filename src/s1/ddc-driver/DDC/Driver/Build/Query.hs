
module DDC.Driver.Build.Query where
import DDC.Driver.Build.State
import DDC.Driver.Build.Taste
import DDC.Driver.Config
import Data.IORef
import System.FilePath
import DDC.Core.Module                          (ModuleName)
import Data.Set                                 (Set)
import qualified DDC.Build.Builder              as Build
import qualified DDC.Build.Interface.Locate     as Build
import qualified Data.Map.Strict                as Map
import qualified Data.Set                       as Set


---------------------------------------------------------------------------------------------------
-- | Check whether we already have an interface loaded into the store.
queryHaveInterface :: State -> ModuleName -> S Bool
queryHaveInterface state nModule
 = do   nsModule  <- liftIO $ readIORef $ stateFactHaveInterface state
        return  $ Set.member nModule nsModule


-- | Check whether we already have the set of interfaces loaded into the store.
queryHaveInterfaces :: State -> Set ModuleName -> S Bool
queryHaveInterfaces state nsModuleWanted
 = do   nsModuleHave  <- liftIO $ readIORef $ stateFactHaveInterface state
        return  $ Set.null $ Set.difference nsModuleWanted nsModuleHave


---------------------------------------------------------------------------------------------------
-- | Determine the file path of a module.
queryLocateModule :: State -> ModuleName -> S FilePath
queryLocateModule state nModule
 = do   pathOfModule    <- liftIO $ readIORef $ stateFactPathOfModule state
        case Map.lookup nModule pathOfModule of
         Just path      -> return path
         Nothing
          -> do path <- locateModuleFromConfig (stateConfig state) nModule
                addFactPathOfModule state nModule path
                return path


-- | Determine the file path of a module, given the driver config.
locateModuleFromConfig :: Config -> ModuleName -> S FilePath
locateModuleFromConfig config mname
 = do   -- Automatically look for modules in the base library.
        let baseDirs
                =  configModuleBaseDirectories config
                ++ [Build.buildBaseSrcDir (configBuilder config) </> "base"]

        (liftIO $ Build.locateModuleFromPaths baseDirs mname "source" ".ds")
         >>= \case
                Left  _err -> error "locateModuleFromConfig: cannot find"
                Right path -> return path


---------------------------------------------------------------------------------------------------
-- | Get the name of the module at the given path.
queryModuleOfPath :: State -> FilePath -> S ModuleName
queryModuleOfPath state filePath
        = fmap fst $ queryTasteModuleAtPath state filePath


-- | Taste the header of a module at the given file path,
--   loading its module name and imports list into the state.
queryTasteModuleAtPath :: State -> FilePath -> S (ModuleName, Set ModuleName)
queryTasteModuleAtPath state filePath
 = goGetName
 where  goGetName
         = do   mnModule <- lookupFactModuleOfPath state filePath
                case mnModule of
                 Nothing      -> goTasteHeader
                 Just nModule -> goGetImports nModule

        goGetImports nModule
         = do   mnImports <- lookupFactModuleImports state nModule
                case mnImports of
                 Nothing       -> goTasteHeader
                 Just nsImport -> return (nModule, nsImport)

        goTasteHeader
         = do
                -- TODO: check file exists.
                -- TODO: avoid reading entire source file as a string.
                -- not sure if the lexer will demand the entire contents of the file.
                str     <- liftIO $ readFile filePath
                Just (nModule, nsImport)
                        <- liftIO $ tasteNeeded filePath str

                addFactModuleOfPath  state filePath nModule
                addFactPathOfModule  state nModule filePath
                addFactModuleImports state nModule (Set.fromList nsImport)

                return (nModule, Set.fromList nsImport)

