
module DDC.Main.Config
        ( Mode   (..)
        , Config (..)
        , defaultConfig
        , parseArgs
        , getDriverConfig)
where
import DDC.Build.Builder
import qualified DDC.Driver.Stage       as D
import qualified DDC.Core.Simplifier    as S
import qualified Data.Map               as Map


-- | The main command that we're running.
data Mode
        -- | Don't do anything
        = ModeNone

        -- | Pretty print a module's AST.
        | ModeAST  FilePath

        -- | Parse and type-check a module.
        | ModeLoad FilePath

        -- | Compile a .dcl or .dce file.
        | ModeCompile FilePath
        deriving (Eq, Show)


-- DDC Config -----------------------------------------------------------------
-- | DDC driver config.
data Config
        = Config
        { configMode    :: Mode }
        deriving (Eq, Show)


-- | Default configuation.
defaultConfig :: Config
defaultConfig
        = Config
        { configMode    = ModeNone }


-- | Parse command line arguments.
parseArgs :: [String] -> Config -> IO Config
parseArgs [] config
        = return config

parseArgs args config
        | "-ast" : file : rest  <- args
        = parseArgs rest
        $ config { configMode   = ModeAST file }

        | "-load" : file : rest <- args
        = parseArgs rest
        $ config { configMode   = ModeLoad file }

        | compile : file : rest <- args
        , elem compile ["-c", "-compile"]
        = parseArgs rest
        $ config { configMode   = ModeCompile file}

        | otherwise
        = error "bad usage"


-- Driver Stage Config --------------------------------------------------------
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

