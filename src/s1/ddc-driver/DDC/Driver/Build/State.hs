
module DDC.Driver.Build.State
        ( ModuleName
        , Job   (..)
        , Error (..)
        , State (..)
        , S
        , newStateOfStore
        , traceBuild
        , addJob, addJobs, takeJob
        , addFactPathOfModule
        , addFactModuleOfPath,  lookupFactModuleOfPath
        , addFactModuleImports, lookupFactModuleImports
        , addFactHaveInterface

        , module Control.Monad
        , module Control.Monad.Trans.Except
        , module Control.Monad.IO.Class)
where
import DDC.Core.Module                          (ModuleName)
import Data.IORef
import Data.Set                                 (Set)
import Data.Map                                 (Map)
import qualified DDC.Build.Interface.Locate     as B
import qualified DDC.Build.Pipeline.Error       as B
import qualified DDC.Driver.Config              as Driver
import qualified DDC.Core.Discus                as Discus
import qualified DDC.Core.Interface.Store       as C
import qualified Data.Map.Strict                as Map
import qualified Data.Set                       as Set

import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.IO.Class


---------------------------------------------------------------------------------------------------
-- | Jobs we need to do in the compilation.
data Job
        -- | Find and build or load a Discus module.
        = JobBuildModuleOfName  ModuleName

        -- | Build the source file at the given path.
        | JobBuildModuleOfPath  FilePath
        deriving (Eq, Ord, Show)


---------------------------------------------------------------------------------------------------
data Error
        = ErrorMissingFile FilePath
        | ErrorLocate       B.ErrorLocate
        | ErrorBuild       [B.Error]
        deriving Show


---------------------------------------------------------------------------------------------------
-- | Builder state.
data State
        = State
        { -- Driver config.
          stateConfig            :: Driver.Config

          -- The interface store.
        , stateStore             :: C.Store Discus.Name

          -- Jobs we still need to do.
        , stateJobs              :: IORef (Set Job, [Job])

          -- Map of module names to paths of source files.
        , stateFactPathOfModule  :: IORef (Map ModuleName FilePath)

          -- Map of paths to module names.
        , stateFactModuleOfPath  :: IORef (Map FilePath ModuleName)

          -- Map of module names to the other modules they directly import.
        , stateFactModuleImports :: IORef (Map ModuleName (Set ModuleName))

          -- Set of module names for which we have interfaces in the interface store.
        , stateFactHaveInterface :: IORef (Set ModuleName)
        }


---------------------------------------------------------------------------------------------------
type S a = ExceptT Error IO a


---------------------------------------------------------------------------------------------------
-- | Construct a builder state from a driver config and interface store.
newStateOfStore
        :: Driver.Config
        -> C.Store Discus.Name
        -> S State

newStateOfStore config store
 = liftIO
 $ do   refJobs                 <- newIORef (Set.empty, [])
        refFactPathOfModule     <- newIORef Map.empty
        refFactModuleOfPath     <- newIORef Map.empty
        refFactModuleImports    <- newIORef Map.empty
        refFactHaveInterface    <- newIORef Set.empty
        return  $ State
                { stateConfig            = config
                , stateStore             = store
                , stateJobs              = refJobs
                , stateFactPathOfModule  = refFactPathOfModule
                , stateFactModuleOfPath  = refFactModuleOfPath
                , stateFactModuleImports = refFactModuleImports
                , stateFactHaveInterface = refFactHaveInterface }


---------------------------------------------------------------------------------------------------
traceBuild :: State -> String -> S ()
traceBuild _state str
 = liftIO $ putStrLn str


---------------------------------------------------------------------------------------------------
-- | Add a job to the end of the list in the state.
addJob :: State -> Job -> S ()
addJob state job
 = do   liftIO $ atomicModifyIORef' (stateJobs state) $ \ss@(sJobs, lJobs)
         -> ( if Set.member job sJobs
                then ss
                else ( Set.insert job sJobs
                     , lJobs ++ [job])
            , ())


-- | Add some jobs to the end of the list in the state.
addJobs :: State -> [Job] -> S ()
addJobs state lJobs'
 = do   let sJobs' = Set.fromList lJobs'
        liftIO $ atomicModifyIORef' (stateJobs state) $ \ss@(sJobs, lJobs)
         -> ( if Set.null (Set.difference sJobs' sJobs)
                then ss
                else ( Set.union sJobs sJobs'
                     , lJobs ++ (Set.toList (Set.difference sJobs' sJobs)))
            , ())


-- | Take a job from the state.
takeJob :: State -> S (Maybe Job)
takeJob state
 = do   liftIO $ atomicModifyIORef' (stateJobs state) $ \ss@(sJobs, lJobs)
         -> case lJobs of
                []      -> (ss, Nothing)
                j : js  -> ((Set.delete j sJobs, js), Just j)


-- PathOfModule -----------------------------------------------------------------------------------
-- | Add the path of a module name to the state.
addFactPathOfModule :: State -> ModuleName -> FilePath -> S ()
addFactPathOfModule state nModule filePath
 = do   liftIO $ atomicModifyIORef' (stateFactPathOfModule state)
         $ \paths -> (Map.insert nModule filePath paths, ())



-- ModuleOfPath -----------------------------------------------------------------------------------
-- | Add the module name of a file path to the state.
addFactModuleOfPath :: State -> FilePath -> ModuleName -> S ()
addFactModuleOfPath state filePath nModule
 = do   liftIO $ atomicModifyIORef' (stateFactModuleOfPath state)
         $ \paths -> (Map.insert filePath nModule paths, ())


-- | Lookup the module name of a file path from the store.
lookupFactModuleOfPath :: State -> FilePath -> S (Maybe ModuleName)
lookupFactModuleOfPath state filePath
 = do   is <- liftIO $ readIORef (stateFactModuleOfPath state)
        return $ Map.lookup filePath is


-- ModuleImports ----------------------------------------------------------------------------------
-- | Add the set of known module imports to the state.
addFactModuleImports :: State -> ModuleName -> Set ModuleName -> S ()
addFactModuleImports state nModule nsModule
 = do   liftIO $ atomicModifyIORef' (stateFactModuleImports state)
         $ \is -> (Map.insert nModule nsModule is, ())


-- | Lookup the set of module imports from the state.
lookupFactModuleImports :: State -> ModuleName -> S (Maybe (Set ModuleName))
lookupFactModuleImports state nModule
 = do   is <- liftIO $ readIORef (stateFactModuleImports state)
        return $ Map.lookup nModule is


-- HaveInterface ----------------------------------------------------------------------------------
-- | Add the fact that we have an interface in the interface store.
addFactHaveInterface :: State -> ModuleName -> S ()
addFactHaveInterface state nModule
 = do   liftIO $ atomicModifyIORef' (stateFactHaveInterface state)
         $ \ints  -> (Set.insert nModule ints, ())

