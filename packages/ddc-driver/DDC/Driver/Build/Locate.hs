
module DDC.Driver.Build.Locate
        (locateModuleFromPaths)
where
import Control.Monad
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import Data.Maybe
import System.FilePath
import qualified System.Directory       as Directory


-- | Locate the source file for a module, starting from the given list of base paths.
--   If the module cannot be found, or is found from multiple paths then throw
--   an error in the monad.
--
locateModuleFromPaths
        :: [FilePath]           -- ^ Base paths.
        -> String               -- ^ Module name.
        -> String               -- ^ Source file extension
        -> ErrorT String IO FilePath

locateModuleFromPaths pathsBase name ext
 = do
        mPaths  <- liftIO 
                $  liftM catMaybes
                $  mapM  (\d -> locateModuleFromPath d name ext) pathsBase

        case mPaths of
         []        -> throwError $ unlines 
                   $  [ "Cannot locate source for module '" ++ name  ++ "' from base directories:" ]
                   ++ ["    " ++ dir | dir <- pathsBase]

         [path]    -> return path

         paths     -> throwError $ unlines
                   $  [ "Source for module '" ++ name ++ "' found at multiple paths:" ]
                   ++ ["    " ++ dir | dir <- paths]


-- | Given the path of a .build spec, and a module name, yield the path where
--   the source of the module should be.
locateModuleFromPath 
        :: FilePath             -- ^ Base path.
        -> String               -- ^ Module name.
        -> String               -- ^ Source file extension.
        -> IO (Maybe FilePath)

locateModuleFromPath pathBase name ext
 = let  go str 
         | elem '.' str
         , (n, '.' : rest)      <- span (/= '.') str
         = n   </> go rest

         | otherwise
         = str <.> ext

   in do
        let pathFile    = pathBase </> go name
        exists          <- Directory.doesFileExist pathFile
        if exists 
         then return $ Just pathFile
         else return Nothing
