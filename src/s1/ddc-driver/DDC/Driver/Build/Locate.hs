
module DDC.Driver.Build.Locate
        (locateModuleFromPaths)
where
import DDC.Core.Module
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.Maybe
import System.FilePath
import qualified DDC.Core.Codec.Text.Pretty        as P
import qualified System.Directory       as Directory


-- | Locate the source file for a module, starting from the given list of base paths.
--   If the module cannot be found, or is found from multiple paths then throw
--   an error in the monad.
--
locateModuleFromPaths
        :: [FilePath]           -- ^ Base paths.
        -> ModuleName           -- ^ Module name.
        -> String               -- ^ Source file extension
        -> ExceptT String IO FilePath

locateModuleFromPaths pathsBase name ext
 = do
        mPaths  <- liftIO
                $  liftM catMaybes
                $  mapM  (\d -> locateModuleFromPath d name ext) pathsBase

        case mPaths of
         []        -> throwE $ unlines
                   $  [ "Cannot locate source for module '"
                                ++ (P.renderIndent $ P.ppr name)
                                ++ "' from base directories:" ]
                   ++ ["    " ++ dir | dir <- pathsBase]

         [path]    -> return path

         paths     -> throwE $ unlines
                   $  [ "Source for module '"
                                ++ (P.renderIndent $ P.ppr name )
                                ++ "' found at multiple paths:" ]
                   ++ ["    " ++ dir | dir <- paths]


-- | Given the path of a .build spec, and a module name, yield the path where
--   the source of the module should be.
locateModuleFromPath
        :: FilePath             -- ^ Base path.
        -> ModuleName           -- ^ Module name.
        -> String               -- ^ Source file extension.
        -> IO (Maybe FilePath)

locateModuleFromPath pathBase (ModuleName parts) ext
 = let
        go []           = ext
        go [p]          = p <.> ext
        go (p : ps)     = p </> go ps

   in do
        let pathFile    = pathBase </> go parts
        exists          <- Directory.doesFileExist pathFile
        if exists
         then return $ Just pathFile
         else return Nothing
