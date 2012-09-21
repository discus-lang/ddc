
module DDC.Main.Config
        ( Mode   (..)
        , Config (..)
        , defaultConfig
        , liteBundleOfConfig
        , saltBundleOfConfig
        , bundleFromFilePath
        , getDriverConfig)
where
import DDC.Build.Builder
import DDC.Build.Language
import DDC.Driver.Bundle
import System.FilePath
import qualified DDC.Driver.Stage       as D
import qualified DDC.Core.Simplifier    as S
import qualified Data.Map               as Map


-- | The main command that we're running.
data Mode
        -- | Don't do anything
        = ModeNone

        -- | Display the help page.
        | ModeHelp

        -- | Pretty print a module's AST.
        | ModeAST  FilePath

        -- | Parse and type-check a module.
        | ModeLoad FilePath

        -- | Compile a .dcl or .dce file.
        | ModeCompile FilePath
        deriving (Eq, Show)


-- | DDC config.
data Config
        = Config
        { configMode    :: Mode }
        deriving (Eq, Show)


-- | Default configuation.
defaultConfig :: Config
defaultConfig
        = Config
        { configMode    = ModeNone }


-- | Get the Lite specific stuff from the config.
liteBundleOfConfig :: Config -> Bundle
liteBundleOfConfig _config
 = Bundle
        { bundleFragment        = fragmentLite
        , bundleModules         = Map.empty
        , bundleStateInit       = ()
        , bundleSimplifier      = S.Trans S.Id
        , bundleRewriteRules    = Map.empty }


-- | Get the Salt specific stuff from the config.
saltBundleOfConfig :: Config -> Bundle
saltBundleOfConfig _config
 = Bundle
        { bundleFragment        = fragmentSalt
        , bundleModules         = Map.empty
        , bundleStateInit       = ()
        , bundleSimplifier      = S.Trans S.Id
        , bundleRewriteRules    = Map.empty }


-- | Determine the current language based on the file extension of this path, 
--   and slurp out a bundle of stuff specific to that language from the config.
bundleFromFilePath :: Config -> FilePath -> Maybe Bundle
bundleFromFilePath config filePath
 = case takeExtension filePath of
        ".dcl"  -> Just (liteBundleOfConfig config)
        ".dce"  -> Just (saltBundleOfConfig config)
        _       -> Nothing


-- | Get the compile driver from the config.
getDriverConfig :: Config -> IO D.Config
getDriverConfig _config
 = do   Just builder <- determineDefaultBuilder defaultBuilderConfig

        return  $ D.Config
                { D.configDump                  = False
                , D.configSimplLite             = S.Trans S.Id
                , D.configSimplSalt             = S.Trans S.Id
                , D.configWithLite              = Map.empty
                , D.configWithSalt              = Map.empty
                , D.configBuilder               = builder
                , D.configSuppressCoreImports   = False
                , D.configSuppressHashImports   = False
                , D.configOutputFile            = Nothing
                , D.configOutputDir             = Nothing }

