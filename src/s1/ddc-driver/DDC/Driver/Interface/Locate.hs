{-# LANGUAGE OverloadedStrings #-}

module DDC.Driver.Interface.Locate
        ( locateModuleFromPaths
        , ErrorLocate (..))
where
import DDC.Core.Module
import DDC.Data.Pretty
import Control.Monad
import Control.Monad.IO.Class
import System.FilePath
import Data.Maybe
import DDC.Driver.Interface.Status              (Status)
import qualified DDC.Driver.Interface.Status    as Status
import qualified Data.Set                       as Set


-------------------------------------------------------------------------------
-- | Things that can go wrong when locating an a file.
data ErrorLocate
        -- | It's just not there..
        = ErrorLocateNotFound
        { errorLocateModuleName :: ModuleName
        , errorLocateBasePaths  :: [FilePath]
        , errorLocateThingName  :: String
        , errorLocateExtension  :: String }

        -- | Too many candidates.
        | ErrorLocateMultiple
        { errorLocateModuleName :: ModuleName
        , errorLocateBasePaths  :: [FilePath]
        , errorLocateThingName  :: String
        , errorLocateExtension  :: String
        , errorLocateFiles      :: [FilePath] }
        deriving Show


instance Pretty ErrorLocate where
 ppr (ErrorLocateNotFound mn paths thing _ext)
  = vcat
  [ text "Cannot locate" %% string thing %% text "for module" %% ppr mn
  , text " when searching from base paths:"
  , indent 4 $ vcat $ map ppr paths ]

 ppr (ErrorLocateMultiple mn paths thing _ext files)
  = vcat
  [ text "Candidate file of" %% string thing %% text "for module" %% ppr mn
  , text " found at multiple paths:"
  , indent 4 $ vcat $ map string files
  , text " when searching from base paths:"
  , indent 4 $ vcat $ map string paths ]


-------------------------------------------------------------------------------
-- | Locate a source file for a module, starting from the given list of base
--   paths. If the module cannot be found, or is found from multiple paths
--   then throw an error in the monad.
locateModuleFromPaths
        :: Status       -- ^ File status cache.
        -> [FilePath]   -- ^ Base paths.
        -> ModuleName   -- ^ Module name.
        -> String       -- ^ Thing name.
        -> String       -- ^ Source file extension
        -> IO (Either ErrorLocate FilePath)

locateModuleFromPaths status pathsBase nModule nThing ext
 = do
        files   <- liftIO
                $  liftM (Set.toList . Set.fromList . catMaybes)
                $  mapM  (\d -> locateModuleFromPath status d nModule ext) pathsBase

        case files of
         []     -> return $ Left
                $  ErrorLocateNotFound nModule pathsBase nThing ext

         [path] -> return $ Right path

         paths  -> return $ Left
                $  ErrorLocateMultiple nModule pathsBase nThing ext paths


-- | Locate a source file for a module, starting from the given base path.
--   If the file cannot be found then `Nothing`.
locateModuleFromPath
        :: Status       -- ^ File status cache.
        -> FilePath     -- ^ Base path.
        -> ModuleName   -- ^ Module name.
        -> String       -- ^ Source file extension.
        -> IO (Maybe FilePath)

locateModuleFromPath status pathBase (ModuleName parts) ext
 = let  go []           = ext
        go [p]          = p <.> ext
        go (p : ps)     = p </> go ps
   in do
        let pathFile    = pathBase </> go parts
        exists <- Status.cachedDoesFileExist status pathFile
        if exists
         then return $ Just pathFile
         else return Nothing

