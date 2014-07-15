
module DDC.Driver.Command.Build
        (cmdBuild)
where
import DDC.Driver.Config
import DDC.Driver.Build.Main
import DDC.Base.Pretty
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import System.FilePath
import qualified DDC.Build.Spec.Parser          as Spec


-- Perform a build following a build specification.
cmdBuild :: Config -> FilePath -> ErrorT String IO ()
cmdBuild config pathSpec

 -- Build from a build spec file
 | ".build"      <- takeExtension pathSpec
 = do
        -- Parse the spec file.
        str     <- liftIO $ readFile pathSpec
        case Spec.parseBuildSpec pathSpec str of
         Left err       -> throwError $ renderIndent $ ppr err
         Right spec     -> buildSpec config pathSpec spec

 -- Don't know how to build from this file.
 | otherwise
 = let  ext     = takeExtension pathSpec
   in   throwError $ "Cannot build from '" ++ ext ++ "' files."

