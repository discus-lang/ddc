
module DDC.Driver.Command.Build
        (cmdBuild)
where
import DDC.Driver.Config
import DDC.Driver.Build.Main
import DDC.Driver.Command.Compile
import DDC.Data.Pretty
import DDC.Build.Interface.Store        (Store)
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import qualified System.FilePath        as FilePath
import qualified DDC.Build.Spec.Parser  as Spec
import qualified DDC.Build.Builder      as Builder
import qualified Data.List              as List
import qualified DDC.Core.Discus        as D


-- Perform a build following a build specification.
cmdBuild :: Config -> Store D.Name -> FilePath -> ExceptT String IO ()
cmdBuild config store filePath

 -- Build from a build spec file
 | ".build"      <- FilePath.takeExtension filePath
 = do
        -- Search for modules in the base library as well as the same directory
        -- the build file is in.
        let config'
                = config
                { configModuleBaseDirectories
                        =  List.nub
                        $  configModuleBaseDirectories config
                        ++ [ FilePath.takeDirectory filePath
                           , Builder.buildBaseSrcDir (configBuilder config)
                                FilePath.</> "base" ]
                }

        -- Parse the spec file.
        str     <- liftIO $ readFile filePath
        case Spec.parseBuildSpec filePath str of
         Left err       -> throwE $ renderIndent $ ppr err
         Right spec     -> buildSpec config' store spec


 -- If we were told to build a source file then just compile it instead.
 -- This is probably the least surprising behaviour.
 | ".ds"        <- FilePath.takeExtension filePath
 = do   cmdCompileRecursive config False store [filePath]
        return ()

 -- Don't know how to build from this file.
 | otherwise
 = let  ext     = FilePath.takeExtension  filePath
   in   throwE $ "Cannot build from '" ++ ext ++ "' files."

